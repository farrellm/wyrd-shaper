-- | WyrdShaper — M4: combat and backlash. Quick slots 1\/2\/3 channel
-- spells over several ticks; chasers charge and hexers channel volleys
-- under the same casting rules as the player; any hit staggers a channel
-- and every collapsed cast backlashes its caster in proportion to the mana
-- already committed. You can lose (and press R about it).
module Wyrdshaper (run) where

import Apecs
import Control.Monad (forM_, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Linear (V2 (..), quadrance)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Wyrdshaper.Editor
import Wyrdshaper.Engine
import Wyrdshaper.Glyph (compile, cycleField, ipPath, modifyAt)
import Wyrdshaper.Loop
import Wyrdshaper.Spell
import Wyrdshaper.Spellbook
import Wyrdshaper.Tilemap
import Wyrdshaper.World

windowW, windowH :: Int
windowW = 800
windowH = 600

-- | Pixels per tick.
playerSpeed :: Int
playerSpeed = 3

-- | Collision box half-extents; every combatant (player, enemy, dummy) is a
-- 24x24 box in 32px tiles.
bodyHalf :: V2 Int
bodyHalf = V2 12 12

boltHalf :: V2 Int
boltHalf = V2 5 5

-- * Shell state

-- | What the player is doing at the meta level. While 'Editing' or
-- 'GameOver', the simulation is frozen: no ticks run.
data Mode = Playing | Editing EditorState | GameOver

-- | Meta-state owned by the loop closures, not the ECS: the current mode
-- and the spellbook. Lives in an 'IORef' (like the demo driver's tick
-- counter) because apecs 'Apecs.Global' stores cannot join
-- 'Wyrdshaper.World.AllComponents'.
data Shell = Shell {shMode :: Mode, shBook :: Spellbook}

-- | The quick slot (if any) tapped this frame, resolved against the
-- player's spellbook.
quickSlot :: Spellbook -> Input -> Maybe Stmt
quickSlot book input
  | tapped Scancode1 = Just (slotSpell 0 book)
  | tapped Scancode2 = Just (slotSpell 1 book)
  | tapped Scancode3 = Just (slotSpell 2 book)
  | otherwise = Nothing
  where
    tapped k = keyTapped k input

-- * Setup

run :: IO ()
run = withEngine "WyrdShaper" (V2 windowW windowH) $ \gfx -> do
  w <- initWorld
  book <- loadSpellbook spellbookPath
  runWith w $ do
    let (tm, _) = worldMap
    playerE <- newEntity (FPlayer)
    let game = Game {gameMap = tm, gamePlayer = playerE}
    spawnLevel game

    shellRef <- liftIO $ newIORef (Shell Playing book)

    -- Headless smoke test: WYRD_DEMO=1 drives the game under Xvfb where no
    -- keyboard or mouse exists. It casts the quick slots on a schedule,
    -- runs a keyboard-style editor pass (glyph ops + commit through the
    -- same path as the Return key), then a mouse pass: synthetic 'Input's
    -- are queued one per frame and fed through the real 'frame' path, so
    -- drag/snap, dropdowns, and drag-to-palette delete are exercised
    -- end-to-end with layout-derived coordinates. An M4 combat pass
    -- follows: held-key walks (reaching the ticks via injRef below), a
    -- deliberate stagger under the chaser's blows, both enemies slain, a
    -- scripted death out east, and an R-tap restart. Scheduled per frame
    -- (not per tick): ticks freeze while editing. Returns the frame's
    -- injected input, if any.
    demoFrame <- do
      mDemo <- liftIO $ lookupEnv "WYRD_DEMO"
      case mDemo of
        Nothing -> pure (pure Nothing)
        Just _ -> do
          frameRef <- liftIO $ newIORef (0 :: Int)
          queueRef <- liftIO $ newIORef ([] :: [Input])
          let castSlot k = do
                sh <- liftIO $ readIORef shellRef
                startCastAt (gamePlayer game) ticksPerInstr (slotSpell k (shBook sh))
              -- Hold movement keys for n frames (the injected input reaches
              -- the ticks through injRef below).
              walk n ks = push (replicate n (demoKeysInput ks))
              logHP tag = do
                Health hp maxHp <- get (gamePlayer game)
                liftIO . hPutStrLn stderr $
                  "demo combat [" ++ tag ++ "]: hp " ++ show hp ++ "/" ++ show maxHp
              openDemoEditor slot editBuf = liftIO $ do
                sh <- liftIO $ readIORef shellRef
                let st = openEditor slot (shBook sh)
                    st' = maybe st (\b -> st {edBuf = b}) (editBuf (edBuf st))
                writeIORef shellRef sh {shMode = Editing st'}
              commitDemoEdit = do
                sh <- liftIO $ readIORef shellRef
                forM_ [st | Editing st <- [shMode sh]] $ \st ->
                  forM_ (compile (edBuf st)) $ \stmt ->
                    liftIO $ commitShell shellRef (edSlot st) stmt

              -- Mouse-gesture plumbing: actions compute coordinates from
              -- the live layout and queue one synthetic input per frame
              -- (drags must stay contiguous or the lost-release guard
              -- rightly cancels them).
              withEditor k = do
                sh <- liftIO $ readIORef shellRef
                case shMode sh of
                  Editing st -> do
                    lay <- buildLayout gfx st
                    k lay st
                  _ -> pure ()
              push ins = liftIO $ modifyIORef' queueRef (++ ins)
              center (Rect (V2 x y) (V2 rw rh)) = V2 (x + rw `div` 2) (y + rh `div` 2)
              clickSeq p = [demoInput p True True False, demoInput p False False True]
              -- Press, glide, then hover at the drop point (long enough
              -- for a screenshot to catch the ghost and snap indicator)
              -- before releasing.
              dragSeq hold from to =
                let steps = 6 :: Int
                    lerp i = from + fmap (\d -> (d * i) `div` steps) (to - from)
                 in [demoInput from True True False]
                      ++ [demoInput (lerp i) True False False | i <- [1 .. steps]]
                      ++ replicate hold (demoInput to True False False)
                      ++ [demoInput to False False True]
              gapAim g = V2 (gbLineX g + 2) (gbY g)
              logBuf tag = withEditor $ \_ st ->
                liftIO $ hPutStrLn stderr ("demo editor [" ++ tag ++ "]: " ++ show (edBuf st))

              -- Drag palette REPEAT to the end of the spell (held a while
              -- mid-flight so a screenshot can catch ghost + snap line).
              dragPaletteRepeat = withEditor $ \lay st ->
                forM_ (find ((== [length (edBuf st)]) . ipPath . gbPoint) (layGaps lay)) $
                  \g -> push (dragSeq 60 (center (snd (layPalette lay !! 3))) (gapAim g))
              -- Click a field piece of the row at a path (opens its menu).
              clickField path f = withEditor $ \lay _ ->
                forM_
                  ( do
                      rb <- find ((== path) . rbPath) (layRows lay)
                      find ((== Just f) . pbField) (rbPieces rb)
                  )
                  (push . clickSeq . center . pbRect)
              clickMenuItem lbl = withEditor $ \lay _ ->
                forM_ (layMenu lay) $ \mb ->
                  forM_ (find (\(_, s, _) -> s == lbl) (mbItems mb)) $
                    \(_, _, r) -> push (clickSeq (center r))
              dragRowToGap src dst = withEditor $ \lay _ ->
                forM_
                  ( (,)
                      <$> find ((== src) . rbPath) (layRows lay)
                      <*> find ((== dst) . ipPath . gbPoint) (layGaps lay)
                  )
                  $ \(rb, g) -> push (dragSeq 0 (center (rbRect rb)) (gapAim g))
              dragRowToPalette src hold = withEditor $ \lay _ ->
                forM_ (find ((== src) . rbPath) (layRows lay)) $ \rb ->
                  push (dragSeq hold (center (rbRect rb)) (V2 100 300))

              script =
                [ (30 :: Int, castSlot 1),
                  (210, castSlot 2),
                  (360, castSlot 0),
                  -- keyboard-style pass: volley, count bumped by a field op
                  (450, openDemoEditor 1 (modifyAt [0] (cycleField [] 0 1))),
                  (600, commitDemoEdit),
                  (650, castSlot 1),
                  -- mouse pass on slot 3 (brand-repel: LET / IF / PUSH / KINDLE)
                  (720, openDemoEditor 2 (const Nothing)),
                  (730, dragPaletteRepeat),
                  (828, logBuf "palette drag"),
                  (830, clickField [1] 0), -- the new REPEAT's count: menu opens
                  (900, clickMenuItem "4"),
                  (918, logBuf "menu pick"),
                  (920, dragRowToGap [0, 1] [1, 0]), -- KINDLE into REPEAT's hole
                  (948, logBuf "kindle move"),
                  (950, dragRowToPalette [0] 30), -- the whole LET subtree: deleted
                  (998, logBuf "let delete"),
                  (1000, commitDemoEdit),
                  (1020, castSlot 2),
                  -- M4 combat pass: step toward the north door; the chaser
                  -- charges in through it and the hexer opens up outside.
                  -- Walk lengths are chosen so the snap-on-stop glide lands
                  -- each stop on the tile center the beat was tuned around.
                  (1100, walk 21 [ScancodeW]), -- 559 -> snaps to 560 (tile 17)
                  (1230, logHP "first blood"),
                  -- channel kindle loops beside the hammering chaser: its
                  -- contact hits land every 45 ticks from ~1206, so the
                  -- first two channels are staggered one instruction in
                  -- (stagger, backlash, red wash) and the third completes
                  -- in the clear window and kindles the doorway
                  (1240, castSlot 2),
                  (1285, castSlot 2),
                  (1305, castSlot 2),
                  -- a volley the 1341 contact hit is sure to stagger, then
                  -- firebolts timed inside the clear window put it down
                  (1336, castSlot 1),
                  (1345, castSlot 0),
                  (1360, castSlot 0),
                  (1375, castSlot 0),
                  (1465, logHP "chaser down"),
                  -- out the door into the hexer's bolt line; volleys at the
                  -- nearest foe cut its channel short
                  (1480, walk 35 [ScancodeW]),
                  (1530, castSlot 1),
                  (1590, castSlot 1),
                  (1650, logHP "north field cleared"),
                  -- the long march east: stand under the far chaser and lose
                  (1680, walk 10 [ScancodeS]), -- 658 -> snaps to 656 (tile 20)
                  (1710, walk 170 [ScancodeD]), -- 974 -> snaps to 976, the far chaser's column
                  (2380, logHP "the end"),
                  -- back from the dead through the real R-key path
                  (2430, push [demoTapInput [ScancodeR]]),
                  (2480, logHP "after restart")
                ]
          pure $ do
            n <- liftIO $ atomicModifyIORef' frameRef (\n -> (n + 1, n))
            forM_ (find ((== n) . fst) script) snd
            liftIO . atomicModifyIORef' queueRef $ \q -> case q of
              [] -> ([], Nothing)
              i : is -> (is, Just i)

    -- Demo input must reach the ticks too (held-key walking), not just the
    -- frame handler; the frame runs before its frame's ticks, so stashing
    -- the injected input here is deterministic.
    injRef <- liftIO $ newIORef (Nothing :: Maybe Input)

    runLoop
      ( \input -> do
          mi <- demoFrame
          liftIO $ writeIORef injRef mi
          frame gfx game shellRef (fromMaybe input mi)
      )
      ( \input -> do
          sh <- liftIO $ readIORef shellRef
          inj <- liftIO $ readIORef injRef
          case shMode sh of
            Editing _ -> pure () -- the world holds its breath
            GameOver -> pure () -- and here it has stopped breathing
            Playing -> tick game (shBook sh) (fromMaybe input inj)
      )
      ( do
          draw gfx game
          sh <- liftIO $ readIORef shellRef
          forM_ [st | Editing st <- [shMode sh]] $ \st -> do
            lay <- buildLayout gfx st
            drawEditor gfx lay st
          case shMode sh of
            GameOver -> drawGameOver gfx
            _ -> pure ()
          presentFrame gfx
      )
      ( \input -> do
          sh <- liftIO $ readIORef shellRef
          -- Esc quits from play and from the death screen; in the editor it
          -- cancels instead (handled by the frame path).
          let escQuits = case shMode sh of Editing _ -> False; _ -> True
          pure (inputQuit input || (escQuits && keyTapped ScancodeEscape input))
      )

-- | Populate (or repopulate) the level: the player's components are re-'set'
-- on the same entity id ('gamePlayer' must stay valid across restarts), and
-- the start room's dummies plus the overworld enemies are spawned fresh.
spawnLevel :: Game -> System' ()
spawnLevel g = do
  let (_, start) = worldMap
  set (gamePlayer g) $
    ( Position start,
      Mana manaMax manaMax 0,
      Facing (V2 0 (-1)),
      Health playerMaxHP playerMaxHP,
      FPlayer
    )

  -- Target dummies on open tiles of the starting room.
  forM_ [V2 18 15, V2 10 13, V2 20 17] $ \txy ->
    newEntity_ (Position (tileCenter txy), Health dummyMaxHP dummyMaxHP, FEnemy, Enemy Dummy 0)

  -- Enemies north of the start room, all outside aggro range of the start
  -- tile so the start room stays a safe workshop until the player leaves.
  spawnFoe Chaser chaserHP (V2 14 23)
  spawnFoe Hexer hexerHP (V2 18 22)
  spawnFoe Chaser chaserHP (V2 30 25)
  where
    spawnFoe kind hp txy = do
      e <-
        newEntity
          ( Position (tileCenter txy),
            Health hp hp,
            FEnemy,
            Enemy kind 0,
            Facing (V2 0 (-1))
          )
      when (kind == Hexer) $ set e (Mana enemyManaMax enemyManaMax 0)

-- | Once-per-frame UI input: opening, driving, and closing the editor.
-- Runs off the frame, not the tick — see 'Wyrdshaper.Loop.runLoop'. Needs
-- 'Gfx' to measure the frame's editor layout for mouse hit-testing.
frame :: Gfx -> Game -> IORef Shell -> Input -> System' ()
frame gfx g shellRef input = do
  sh <- liftIO $ readIORef shellRef
  case shMode sh of
    Playing -> do
      Health hp _ <- get (gamePlayer g)
      if hp <= 0
        then do
          -- Mode flips are frame-side, like all meta transitions; the tick
          -- pipeline already stops itself on a dead player.
          liftIO $ hPutStrLn stderr "combat: GAME OVER"
          liftIO $ writeIORef shellRef sh {shMode = GameOver}
        else when (keyTapped ScancodeE input) $ do
          mCast <- get (gamePlayer g)
          case (mCast :: Maybe Casting) of
            Just _ -> pure () -- no leafing through the book mid-incantation
            Nothing ->
              liftIO $ writeIORef shellRef sh {shMode = Editing (openEditor 0 (shBook sh))}
    Editing st -> do
      lay <- buildLayout gfx st
      case updateEditor lay input (shBook sh) st of
        EdContinue st' -> liftIO $ writeIORef shellRef sh {shMode = Editing st'}
        EdCancel -> liftIO $ writeIORef shellRef sh {shMode = Playing}
        EdCommit slot stmt -> liftIO $ commitShell shellRef slot stmt
    GameOver ->
      when (keyTapped ScancodeR input) $ do
        restartGame g
        liftIO $ hPutStrLn stderr "combat: restarted"
        liftIO $ writeIORef shellRef sh {shMode = Playing}

-- | Land a finished spell: write it into the book, persist the book, and
-- return to play. The one path a spell takes from editor to quick slot,
-- keyboard- and demo-driven alike.
commitShell :: IORef Shell -> Int -> Stmt -> IO ()
commitShell shellRef slot stmt = do
  sh <- readIORef shellRef
  let book' = setSlot slot stmt (shBook sh)
  saveSpellbook spellbookPath book'
  writeIORef shellRef (Shell Playing book')

-- * Tick systems

tick :: Game -> Spellbook -> Input -> System' ()
tick g book input = do
  Health hp _ <- get (gamePlayer g)
  when (hp > 0) $ do
    tickInput g book input
    tickEnemies g
    tickCast g
    tickProjectiles g
    tickBurning
    tickRegen
    tickTimers

-- | Movement and cast start. Channeling roots the player: no movement, no
-- new casts.
tickInput :: Game -> Spellbook -> Input -> System' ()
tickInput g book input = do
  mCast <- get (gamePlayer g)
  case (mCast :: Maybe Casting) of
    Just _ -> pure ()
    Nothing -> do
      let axis neg neg' pos pos' =
            (if keyHeld pos input || keyHeld pos' input then 1 else 0)
              - (if keyHeld neg input || keyHeld neg' input then 1 else 0)
          dir = V2 (axis ScancodeA ScancodeLeft ScancodeD ScancodeRight) (axis ScancodeS ScancodeDown ScancodeW ScancodeUp)
          -- crude diagonal compensation: 2px on both axes vs 3px on one
          speed = case dir of V2 x y | x /= 0 && y /= 0 -> 2; _ -> playerSpeed
      Position p <- get (gamePlayer g)
      Facing f <- get (gamePlayer g)
      let delta
            | dir /= V2 0 0 = fmap (* speed) dir
            -- No input: glide onto the grid, clamping the last step so the
            -- glide lands exactly on a tile center without overshooting.
            | otherwise =
                fmap (\c -> signum c * min playerSpeed (abs c)) (snapTarget f p - p)
      set (gamePlayer g) $
        Position (moveAndCollide (gameMap g) bodyHalf p delta)
      forM_ [dir | dir /= V2 0 0] $ \d ->
        set (gamePlayer g) (Facing d)
      forM_ (quickSlot book input) (startCastAt (gamePlayer g) ticksPerInstr)

-- | Where the player settles when movement input stops: per axis, the next
-- tile center along the last motion direction — never behind, so stopping
-- reads as carrying momentum forward, not hopping back. An axis that wasn't
-- moving keeps its containing tile's center. A forward center blocked by a
-- wall (the player walked flush into it) just leaves the glide stopped
-- against the wall.
snapTarget :: V2 Int -> V2 Int -> V2 Int
snapTarget dir p = snapAxis <$> dir <*> p
  where
    half = tileSize `div` 2
    snapAxis s x
      | s > 0 = tileSize * ((x - half + tileSize - 1) `div` tileSize) + half
      | s < 0 = tileSize * ((x - half) `div` tileSize) + half
      | otherwise = tileSize * (x `div` tileSize) + half

-- | Begin channeling a spell on any caster at its speaking pace (no-op
-- guard against re-entry is the caller's job; every entry point checks for
-- an existing cast). Refuses anything over the Willpower budget — the
-- editor and loader enforce it too, but nothing may channel past it
-- regardless of where the spell came from.
startCastAt :: Entity -> Int -> Stmt -> System' ()
startCastAt e pace s
  | spellSize s > willpowerMax =
      liftIO $ hPutStrLn stderr ("cast refused: spell exceeds Willpower " ++ show willpowerMax)
  | otherwise =
      set e . Casting $
        CastState
          { castVM = newVM s,
            castCooldown = pace,
            castSpent = 0,
            castSize = spellSize s,
            castPace = pace
          }

-- | Simple enemy AI. Chasers run at the player and strike on contact;
-- hexers stand and channel 'hexerSpell' when the player draws near — under
-- the same casting rules as the player, so hitting one mid-channel staggers
-- it into its own backlash. Dummies just stand there, taking it.
tickEnemies :: Game -> System' ()
tickEnemies g = do
  Position pp <- get (gamePlayer g)
  cmapM_ $ \(Enemy kind cd, Position p, e) -> case kind of
    Dummy -> pure ()
    Chaser -> do
      when (quadrance (pp - p) <= chaserAggro * chaserAggro) $
        set e (Position (moveAndCollide (gameMap g) bodyHalf p (velToward enemySpeed p pp)))
      Position p' <- get e
      let V2 dx dy = abs <$> (pp - p')
          touching = dx < 24 && dy < 24
      if cd > 0
        then set e (Enemy Chaser (cd - 1))
        else when touching $ do
          damageEntity g (gamePlayer g) contactDamage
          set e (Enemy Chaser contactCooldownTicks)
    Hexer -> do
      mCast <- get e
      case (mCast :: Maybe Casting) of
        Just _ -> pure () -- rooted while channeling, like the player
        Nothing
          | cd > 0 -> set e (Enemy Hexer (cd - 1))
          | quadrance (pp - p) <= hexerAggro * hexerAggro -> do
              set e (Facing (signum <$> (pp - p)))
              startCastAt e enemyTicksPerInstr hexerSpell
              set e (Enemy Hexer hexerRecastTicks)
              liftIO $ hPutStrLn stderr "combat: enemy cast started"
          | otherwise -> pure ()

-- | Advance every channeling spell: each caster, every 'castPace' ticks,
-- charges one mana, runs one VM instruction against a fresh snapshot of its
-- own foes, and applies the effects. Player and enemy casters run through
-- this same code — same rules, same collapses.
tickCast :: Game -> System' ()
tickCast g = cmapM_ $ \(Casting cs, fac :: Faction, e) ->
  if castCooldown cs > 1
    then set e (Casting cs {castCooldown = castCooldown cs - 1})
    else do
      Mana mana cap clock <- get e
      if mana <= 0
        then fizzleCast g e cs (show OutOfMana)
        else do
          set e (Mana (mana - 1) cap clock)
          foes <- foeSnapshot fac
          Position pos <- get e
          Facing facing <- get e
          let wv =
                WorldView
                  { wvCaster = pos,
                    wvFacing = facing,
                    wvMana = mana - 1,
                    wvFoes = [(unEntity fe, p) | (fe, p) <- foes]
                  }
          case step wv (castVM cs) of
            Fizzle err -> fizzleCast g e cs (show err)
            Done effs -> do
              mapM_ (applyEffect g e fac foes) effs
              destroy e (Proxy @Casting)
            Continue vm' effs -> do
              mapM_ (applyEffect g e fac foes) effs
              set e . Casting $
                cs
                  { castVM = vm',
                    castCooldown = castPace cs,
                    castSpent = castSpent cs + 1
                  }

-- | A collapsed cast: the spell drops and the wild energy already committed
-- bites its caster — 'backlashDamage' of the mana spent. Raw 'hurt', not
-- 'damageEntity': backlash is self-inflicted, so it ignores i-frames (the
-- stagger that caused it may just have granted some) and cannot re-stagger.
fizzleCast :: Game -> Entity -> CastState -> String -> System' ()
fizzleCast g e cs why = do
  let dmg = backlashDamage (castSpent cs)
      who = if e == gamePlayer g then "player" else "enemy"
  destroy e (Proxy @Casting) -- first, so the backlash finds no cast to stagger
  liftIO . hPutStrLn stderr $
    "combat: " ++ who ++ " cast fizzled (" ++ why ++ "), backlash " ++ show dmg
  hurt g e dmg backlashFlashTicks

-- | Live combatants of the other side, with positions: what a caster's
-- selectors can target and what its bolts can hit.
foeSnapshot :: Faction -> System' [(Entity, V2 Int)]
foeSnapshot fac =
  cfold
    (\acc (Health hp _, Position p, fac' :: Faction, e) -> if fac' /= fac && hp > 0 then (e, p) : acc else acc)
    []

-- | Apply one 'Effect' on behalf of its caster: bolts carry the caster's
-- faction, and self-directed shoves move the caster, whoever that is.
applyEffect :: Game -> Entity -> Faction -> [(Entity, V2 Int)] -> Effect -> System' ()
applyEffect g caster fac foes eff = case eff of
  SpawnBolt from vel -> newEntity_ (Position from, Projectile vel boltTTL, fac)
  PushEff target delta -> case target of
    TSelf -> shove caster delta
    TFoe fid -> forM_ (find ((== fid) . unEntity . fst) foes) $ \(e, _) -> shove e delta
    TTile _ -> pure () -- the ground declines to move
  KindleEff txy -> do
    burning <- cfold (\acc (Burning t _, e) -> if t == txy then e : acc else acc) []
    case burning of
      e : _ -> set e (Burning txy burnTicks) -- refresh, don't stack
      [] -> newEntity_ (Burning txy burnTicks)
  where
    shove e delta = do
      mp <- get e
      forM_ (mp :: Maybe Position) $ \(Position p) ->
        set e (Position (moveAndCollide (gameMap g) bodyHalf p delta))

-- | Fly bolts, expire them on walls and time, land hits on the other side.
tickProjectiles :: Game -> System' ()
tickProjectiles g =
  cmapM_ $ \(Projectile vel ttl, Position p, fac :: Faction, e) -> do
    let p' = p + vel
    if ttl <= 0 || boxHitsSolid (gameMap g) boltHalf p'
      then destroyEntity e
      else do
        foes <- foeSnapshot fac
        let V2 ox oy = boltHalf + bodyHalf
            hit (_, V2 dx dy) =
              let V2 px py = p'
               in abs (px - dx) < ox && abs (py - dy) < oy
        case find hit foes of
          Just (te, _) -> do
            damageEntity g te boltDamage
            destroyEntity e -- the bolt is spent even if i-frames ate the hit
          Nothing -> set e (Position p', Projectile vel (ttl - 1))

-- | Raw damage: HP math, hit flash, and death. No i-frame gate and no
-- stagger check — backlash routes here so a collapsing cast always bites.
-- A no-op on entities without 'Health' (already slain this tick).
hurt :: Game -> Entity -> Int -> Int -> System' ()
hurt g e dmg flashTicks = do
  mhp <- get e
  forM_ (mhp :: Maybe Health) $ \(Health cur maxHp) ->
    if e == gamePlayer g
      then do
        -- The player is never destroyed: HP clamps at 0 and the frame
        -- handler turns a dead player into the GameOver mode.
        set e (Health (max 0 (cur - dmg)) maxHp, HitFlash flashTicks)
        liftIO . hPutStrLn stderr $
          "combat: player hit for " ++ show dmg ++ ", hp " ++ show (max 0 (cur - dmg)) ++ "/" ++ show maxHp
      else
        if cur <= dmg
          then do
            liftIO $ hPutStrLn stderr "combat: enemy slain"
            destroyEntity e
          else set e (Health (cur - dmg) maxHp, HitFlash flashTicks)

-- | Damage through the combat rules: player i-frames can negate it, and a
-- hit at or over 'staggerThreshold' on a channeling caster — player or
-- enemy — collapses the cast into backlash on top of the hit.
damageEntity :: Game -> Entity -> Int -> System' ()
damageEntity g e dmg = do
  mInv <- get e
  case (mInv :: Maybe Invuln) of
    Just _ -> pure ()
    Nothing -> do
      wasCasting <- get e -- before 'hurt': the hit may destroy the entity
      hurt g e dmg hitFlashTicks
      when (e == gamePlayer g) $ set e (Invuln invulnTicks)
      forM_ (wasCasting :: Maybe Casting) $ \(Casting cs) ->
        when (dmg >= staggerThreshold) $
          fizzleCast g e cs "staggered"

-- | Burn down fires; expired ones vanish.
tickBurning :: System' ()
tickBurning = cmapM_ $ \(Burning txy left, e) ->
  if left <= 1
    then destroyEntity e
    else set e (Burning txy (left - 1))

-- | One mana back every 'manaRegenTicks' ticks, up to each caster's cap.
tickRegen :: System' ()
tickRegen = cmap $ \(Mana cur cap clock) ->
  if clock + 1 >= manaRegenTicks
    then Mana (min cap (cur + 1)) cap 0
    else Mana cur cap (clock + 1)

-- | Count down transient statuses: hit flashes and player i-frames.
tickTimers :: System' ()
tickTimers = do
  cmapM_ $ \(HitFlash n, e) ->
    if n <= 1 then destroy e (Proxy @HitFlash) else set e (HitFlash (n - 1))
  cmapM_ $ \(Invuln n, e) ->
    if n <= 1 then destroy e (Proxy @Invuln) else set e (Invuln (n - 1))

-- | Death is not the end: clear the board and speak the level anew. The
-- player keeps their entity id (and spellbook); everything else respawns.
restartGame :: Game -> System' ()
restartGame g = do
  cmapM_ $ \(Position _, e) -> destroyEntity e
  cmapM_ $ \(Burning _ _, e) -> destroyEntity e -- fires carry no Position
  spawnLevel g

-- * Drawing

-- | Render with the camera on the player, clamped to the map edges; HUD
-- bars for mana, cast progress (while channeling), and health. Draw order
-- is explicit back-to-front: tiles, player, enemies, bolts, fire, HUD,
-- damage wash. The caller presents the frame (the editor panel and the
-- game-over veil, when applicable, draw on top).
draw :: Gfx -> Game -> System' ()
draw gfx g = do
  clearFrame gfx (Color 0.03 0.03 0.05 1)
  Position p <- get (gamePlayer g)
  let cam = clampCamera (gameMap g) p
      tileColor t = case t of
        Floor -> Color 0.16 0.22 0.14 1
        Wall -> Color 0.46 0.43 0.38 1
        Water -> Color 0.13 0.25 0.48 1
      castFrac cs = fromIntegral (castSpent cs) / fromIntegral (max 1 (castSize cs)) :: Float
  forM_ (tiles (gameMap g)) $ \(txy, t) ->
    fillWorldRect gfx cam (tileCenter txy) (V2 tileSize tileSize) (tileColor t)

  -- The player: white while hit-flashing, blinking translucent during
  -- i-frames, ember orange otherwise.
  mFlash <- get (gamePlayer g)
  mInv <- get (gamePlayer g)
  let playerColor
        | Just (HitFlash _) <- mFlash :: Maybe HitFlash = Color 1 1 1 1
        | Just (Invuln n) <- mInv :: Maybe Invuln, odd (n `div` 4) = Color 0.9 0.4 0.1 0.5
        | otherwise = Color 0.9 0.4 0.1 1
  fillWorldRect gfx cam p (V2 24 24) playerColor

  -- Enemies (dummies included), with hurt-only HP bars and, while one is
  -- channeling, the gold cast bar that telegraphs the interrupt window.
  cmapM_ $ \(Enemy kind _, Health cur maxHp, Position ep, e) -> do
    mf <- get e
    let base = case kind of
          Dummy -> Color 0.62 0.45 0.25 1
          Chaser -> Color 0.75 0.15 0.15 1
          Hexer -> Color 0.55 0.25 0.75 1
        col = case mf :: Maybe HitFlash of
          Just _ -> Color 1 1 1 1
          Nothing -> base
    fillWorldRect gfx cam ep (V2 24 24) col
    when (cur < maxHp) $
      drawWorldBar gfx cam (ep + V2 0 20) (fromIntegral cur / fromIntegral maxHp) (Color 0.85 0.2 0.2 1)
    mc <- get e
    forM_ (mc :: Maybe Casting) $ \(Casting cs) ->
      drawWorldBar gfx cam (ep + V2 0 27) (castFrac cs) (Color 0.95 0.8 0.3 1)

  -- Bolts, colored by whose word they carry.
  cmapM_ $ \(Projectile _ _, Position bp, fac :: Faction) ->
    fillWorldRect gfx cam bp (V2 10 10) $ case fac of
      FPlayer -> Color 1.0 0.85 0.3 1
      FEnemy -> Color 0.9 0.3 0.55 1
  cmapM_ $ \(Burning txy _) ->
    fillWorldRect gfx cam (tileCenter txy) (V2 28 28) (Color 0.95 0.35 0.1 1)

  -- HUD: mana, cast progress while channeling, health.
  Mana cur cap _ <- get (gamePlayer g)
  drawHudBar gfx 0 (fromIntegral cur / fromIntegral cap) (Color 0.25 0.55 0.95 1)
  mCast <- get (gamePlayer g)
  forM_ (mCast :: Maybe Casting) $ \(Casting cs) ->
    drawHudBar gfx 1 (castFrac cs) (Color 0.95 0.8 0.3 1)
  Health hp maxHp <- get (gamePlayer g)
  drawHudBar gfx 2 (fromIntegral hp / fromIntegral maxHp) (Color 0.85 0.2 0.2 1)

  -- Full-screen damage wash while the player's hit flash runs: a blip for
  -- ordinary hits, an unmistakable red wave for backlash (longer flash).
  forM_ (mFlash :: Maybe HitFlash) $ \(HitFlash n) ->
    fillUiRect
      gfx
      (V2 0 0)
      (V2 windowW windowH)
      (Color 0.8 0.1 0.1 (0.35 * fromIntegral n / fromIntegral backlashFlashTicks))

-- | A small status bar hovering in world space over a combatant (enemy HP,
-- the cast telegraph): dark backing, left-aligned fill.
drawWorldBar :: Gfx -> V2 Int -> V2 Int -> Float -> Color -> System' ()
drawWorldBar gfx cam center frac color = do
  fillWorldRect gfx cam center (V2 26 5) (Color 0.08 0.08 0.1 0.85)
  let w = round (24 * max 0 (min 1 frac)) :: Int
  when (w > 0) $
    fillWorldRect gfx cam (center + V2 ((w - 24) `div` 2) 0) (V2 w 3) color

-- | The lose screen: a dark veil over the world's last moment.
drawGameOver :: Gfx -> System' ()
drawGameOver gfx = do
  fillUiRect gfx (V2 0 0) (V2 windowW windowH) (Color 0 0 0 0.6)
  let title = "YOU DIED"
      hint = "R to restart - Esc to quit"
  V2 tw th <- measureText gfx TitleFont title
  drawText gfx TitleFont (V2 ((windowW - tw) `div` 2) (windowH `div` 2 - th)) (Color 0.85 0.15 0.15 1) title
  V2 hw _ <- measureText gfx TextFont hint
  drawText gfx TextFont (V2 ((windowW - hw) `div` 2) (windowH `div` 2 + 12)) (Color 0.85 0.8 0.75 1) hint

clampCamera :: Tilemap -> V2 Int -> V2 Int
clampCamera tm (V2 x y) =
  V2
    (clampAxis (windowW `div` 2) (mapWidth tm * tileSize - windowW `div` 2) x)
    (clampAxis (windowH `div` 2) (mapHeight tm * tileSize - windowH `div` 2) y)
  where
    clampAxis lo hi v
      | lo > hi = (lo + hi) `div` 2 -- map smaller than the window: center it
      | otherwise = max lo (min hi v)
