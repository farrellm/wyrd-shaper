-- | WyrdShaper — M5: procgen. A seed speaks the whole world: a noise-biome
-- overworld with the hand-authored start area stamped in whole, a road to
-- a sunken dungeon, and a room-graph dungeon whose locked door demands all
-- four antechamber torches burn at once — a puzzle whose intended key is a
-- loop (@REPEAT 4 { KINDLE UNLIT TORCH }@), because the unlit-torch
-- selector re-resolves every iteration. Combat, backlash, and the M4 rules
-- carry over unchanged; stairs swap levels, and dying anywhere restarts on
-- the overworld.
module Wyrdshaper (run) where

import Apecs
import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Linear (V2 (..), quadrance)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Wyrdshaper.Editor
import Wyrdshaper.Engine
import Wyrdshaper.Glyph (Arg (..), ENode (..), compile, cycleField, ipPath, modifyAt)
import Wyrdshaper.Loop
import Wyrdshaper.Spell
import Wyrdshaper.Spellbook
import Wyrdshaper.Terrain
import Wyrdshaper.Tilemap
import Wyrdshaper.World
import Wyrdshaper.Worldgen

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

-- | The seed every WYRD_DEMO run generates from: the demo's walk segments
-- are tuned frame-exactly against this world.
demoSeed :: Seed
demoSeed = 2026

-- | WYRD_DEMO forces 'demoSeed'; otherwise WYRD_SEED picks the world, and
-- absent both the SDL clock supplies one. Always logged, so any run can be
-- reproduced.
resolveSeed :: IO Seed
resolveSeed = do
  mDemo <- lookupEnv "WYRD_DEMO"
  mSeed <- lookupEnv "WYRD_SEED"
  seed <- case (mDemo, mSeed >>= readMaybe) of
    (Just _, _) -> pure demoSeed
    (_, Just s) -> pure s
    _ -> round . (* 1e9) <$> now
  hPutStrLn stderr ("worldgen: seed " ++ show seed)
  pure seed

run :: IO ()
run = withEngine "WyrdShaper" (V2 windowW windowH) $ \gfx -> do
  w <- initWorld
  terrain <- loadTerrain gfx
  book <- loadSpellbook spellbookPath
  seed <- resolveSeed
  let overworld = generateOverworld (mix64 (seed + 1))
      dungeon = generateDungeon (mix64 (seed + 2))
  runWith w $ do
    playerE <- newEntity (FPlayer)
    levelRef <-
      liftIO . newIORef $
        Level InOverworld (owMap overworld) False False
    let game =
          Game
            { gamePlayer = playerE,
              gameOverworld = overworld,
              gameDungeon = dungeon,
              gameLevel = levelRef
            }
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
              logPos tag = do
                Position p <- get (gamePlayer game)
                liftIO . hPutStrLn stderr $
                  "demo pos [" ++ tag ++ "]: tile " ++ show (fmap (`div` tileSize) p) ++ " px " ++ show p
              -- Hold a direction long enough to land 1-4 px short of the
              -- t-th tile center ahead; the snap-on-stop glide finishes the
              -- step, so every leg ends dead on a tile center.
              legFrames t = (tileSize * t - 2) `div` 3
              -- A run of straight legs starting at a frame: one walk per
              -- leg, the next scheduled after the previous plus glide room.
              walkPlan f0 lgs = snd (foldl planLeg (f0, []) lgs)
                where
                  planLeg (f, acc) (ks, t) =
                    let n = legFrames t
                     in (f + n + 12, acc ++ [(f, walk n ks)])
              openDemoEditor slot editBuf = liftIO $ do
                sh <- liftIO $ readIORef shellRef
                -- The scripted passes aim pixels at the classic row layout;
                -- forcing the view keeps them byte-identical to pre-block runs.
                let st = (openEditor slot (shBook sh)) {edView = ViewRows}
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
                  ++ m5Script

              -- M5 pass (~2560-5600): write the torch spell, march down
              -- the road to the dungeon, bounce out and back through the
              -- stairs, wind through the room graph to the antechamber,
              -- light all four torches with one looped cast (the door
              -- grinds open), over-loop once to eat the NoTarget backlash,
              -- and walk through the door to the shrine. Leg lists are
              -- read off the demoSeed maps (scratchpad Legs.hs method);
              -- retune them if any worldgen constant changes.
              m5Script =
                [ (2560 :: Int, openDemoEditor 2 (const (Just [ERepeat 4 [EInvoke Kindle (ASel UnlitTorch)]]))),
                  (2575, logBuf "m5 torch spell"),
                  (2600, commitDemoEdit)
                ]
                  -- out the start room, around the interior box, through
                  -- the south gap, then the road's two legs onto the
                  -- entrance stairs (the last leg crosses onto them).
                  ++ walkPlan
                    2650
                    [ ([ScancodeS], 5),
                      ([ScancodeD], 13),
                      ([ScancodeS], 9),
                      ([ScancodeA], 3),
                      ([ScancodeS], 42),
                      ([ScancodeD], 37)
                    ]
                  ++ [ -- The stairs-crossing leg can leave residual held
                       -- frames after the transition (frame/tick drift), so
                       -- the landing tile varies; walking into the entry
                       -- room's east wall clamps every variant to the same
                       -- pinned spot before anything is tuned from it.
                       (3880, walk 40 [ScancodeD]),
                       (3930, logPos "dungeon entry"),
                       -- deliberate stair bounce from the normalized spot:
                       -- out to the overworld beside the entrance, and the
                       -- westward glide carries us right back down — both
                       -- transitions exercised, and enterDungeon's exact
                       -- placement re-centers us on the entry tile. 42 is
                       -- the middle of the [40,44] window where the held
                       -- frames end between the exit placement and the
                       -- stairs, leaving the crossing to the glide (shorter
                       -- never leaves the placement center, longer bounces
                       -- a second time on held frames alone).
                       (3945, walk 42 [ScancodeA]),
                       (4010, logPos "after stair bounce")
                     ]
                  -- entry room to the torch antechamber, the long way the
                  -- room tree winds (the first leg steps north off the
                  -- stairs row before heading west past them).
                  ++ walkPlan
                    4030
                    [ ([ScancodeW], 1),
                      ([ScancodeA], 2),
                      ([ScancodeW], 2),
                      ([ScancodeA], 13),
                      ([ScancodeW], 14),
                      ([ScancodeA], 3),
                      ([ScancodeW], 10),
                      ([ScancodeD], 7),
                      ([ScancodeW], 3),
                      ([ScancodeD], 12),
                      ([ScancodeS], 12),
                      ([ScancodeD], 1),
                      ([ScancodeS], 3),
                      ([ScancodeD], 17),
                      ([ScancodeW], 1)
                    ]
                  ++ [ (5280, logPos "antechamber"),
                       (5290, castSlot 2), -- REPEAT 4 KINDLE UNLIT TORCH
                       (5370, castSlot 2) -- every torch burning: backlash
                     ]
                  -- through the opened door to the shrine.
                  ++ walkPlan
                    5420
                    [ ([ScancodeW], 15),
                      ([ScancodeD], 1),
                      ([ScancodeW], 2)
                    ]
                  ++ [(5660, logHP "the wyrd is shaped")]
                  -- block-view pass: the editor opens in the (forced) classic
                  -- view, the real V key flips it to blocks, and one palette
                  -- drag runs against the block geometry; Esc cancels, so the
                  -- spellbook is untouched. Appended after every asserted
                  -- log line — the pre-block stderr stays a byte-identical
                  -- prefix.
                  ++ [ (5700, openDemoEditor 2 (const Nothing)),
                       (5706, push [demoTapInput [ScancodeV]]),
                       (5720, dragPaletteRepeat),
                       (5810, logBuf "block drag"),
                       (5820, push [demoTapInput [ScancodeEscape]])
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
          draw gfx terrain game
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

-- | The tilemap the player is currently standing in.
curMap :: Game -> System' Tilemap
curMap g = lvMap <$> liftIO (readIORef (gameLevel g))

-- | Populate (or repopulate) the overworld: the player's components are
-- re-'set' on the same entity id ('gamePlayer' must stay valid across
-- restarts), and the authored start area's dummies and foes — at their
-- stamped positions — plus the biome scatter are spawned fresh.
spawnLevel :: Game -> System' ()
spawnLevel g = do
  let ow = gameOverworld g
  liftIO . writeIORef (gameLevel g) $
    Level InOverworld (owMap ow) False False
  set (gamePlayer g) $
    ( Position (owStart ow),
      Mana manaMax manaMax 0,
      Facing (V2 0 (-1)),
      Health playerMaxHP playerMaxHP,
      FPlayer,
      Anim (owStart ow) 0 False
    )
  spawnOverworldFoes g

-- | The overworld's population, shared by fresh spawns, restarts, and
-- returns from the dungeon (which reset the level but not the player).
spawnOverworldFoes :: Game -> System' ()
spawnOverworldFoes g = do
  let ow = gameOverworld g
      stamped txy = owStampOrigin ow + txy

  -- Target dummies on open tiles of the starting room.
  forM_ [V2 18 15, V2 10 13, V2 20 17] $ \txy ->
    newEntity_
      ( Position (tileCenter (stamped txy)),
        Health dummyMaxHP dummyMaxHP,
        FEnemy,
        Enemy Dummy 0,
        Facing (V2 0 (-1)),
        Anim (tileCenter (stamped txy)) (spawnClock (stamped txy)) False
      )

  -- Enemies north of the start room, all outside aggro range of the start
  -- tile so the start room stays a safe workshop until the player leaves.
  spawnFoe Chaser chaserHP (stamped (V2 14 23))
  spawnFoe Hexer hexerHP (stamped (V2 18 22))
  spawnFoe Chaser chaserHP (stamped (V2 30 25))

  -- The seed's biome scatter, everywhere the stamp's calm doesn't reach.
  forM_ (owSpawns ow) $ \(kind, txy) -> case kind of
    SpawnChaser -> spawnFoe Chaser chaserHP txy
    SpawnHexer -> spawnFoe Hexer hexerHP txy

-- | Clear a level's population for a transition: every positioned entity
-- except the player, plus the positionless fires. Any channel the player
-- somehow carries across dies quietly (movement is rooted while casting,
-- so stepping onto stairs mid-cast shouldn't be possible — this is a belt
-- for that suspender).
levelTeardown :: Game -> System' ()
levelTeardown g = do
  cmapM_ $ \(Position _, e) -> when (e /= gamePlayer g) $ destroyEntity e
  cmapM_ $ \(Burning _ _, e) -> destroyEntity e
  destroy (gamePlayer g) (Proxy @Casting)

-- | Step onto 'StairsDown': trade the overworld for the dungeon. The
-- player keeps HP, mana, and facing — only the ground changes.
enterDungeon :: Game -> System' ()
enterDungeon g = do
  levelTeardown g
  let dg = gameDungeon g
  liftIO . writeIORef (gameLevel g) $ Level InDungeon (dgMap dg) False False
  set (gamePlayer g) (Position (tileCenter (dgEntry dg)))
  forM_ (dgTorches dg) $ \txy ->
    newEntity_ (Position (tileCenter txy), Torch 0)
  forM_ (dgSpawns dg) $ \(kind, txy) -> case kind of
    SpawnChaser -> spawnFoe Chaser chaserHP txy
    SpawnHexer -> spawnFoe Hexer hexerHP txy
  liftIO $ hPutStrLn stderr "dungeon: entered the dungeon"

-- | Step onto 'StairsUp': back to daylight, beside the entrance. The
-- overworld's population respawns from the seed (regenerate-from-seed;
-- nothing out here persists), and re-entering the dungeon later re-locks
-- the door the same way.
exitDungeon :: Game -> System' ()
exitDungeon g = do
  levelTeardown g
  let ow = gameOverworld g
  liftIO . writeIORef (gameLevel g) $ Level InOverworld (owMap ow) False False
  set (gamePlayer g) (Position (tileCenter (owEntrance ow + V2 1 0)))
  spawnOverworldFoes g
  liftIO $ hPutStrLn stderr "dungeon: returned to the overworld"

spawnFoe :: EnemyKind -> Int -> V2 Int -> System' ()
spawnFoe kind hp txy = do
  e <-
    newEntity
      ( Position (tileCenter txy),
        Health hp hp,
        FEnemy,
        Enemy kind 0,
        Facing (V2 0 (-1)),
        Anim (tileCenter txy) (spawnClock txy) False
      )
  when (kind == Hexer) $ set e (Mana enemyManaMax enemyManaMax 0)

-- | Phase a spawn's free-running animation clock off its tile so mobs
-- don't bob and march in lockstep.
spawnClock :: V2 Int -> Int
spawnClock (V2 x y) =
  fromIntegral (mix64 (mix64 (fromIntegral x) + fromIntegral y)) `mod` 80

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
    tickTorches g
    tickRegen
    tickTimers
    tickAnim
    tickLevel g

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
      tm <- curMap g
      let delta
            | dir /= V2 0 0 = fmap (* speed) dir
            -- No input: glide onto the grid, clamping the last step so the
            -- glide lands exactly on a tile center without overshooting.
            | otherwise =
                fmap (\c -> signum c * min playerSpeed (abs c)) (snapTarget f p - p)
      set (gamePlayer g) $
        Position (moveAndCollideCentered tm bodyHalf p delta)
      forM_ [dir | dir /= V2 0 0] $ \d ->
        set (gamePlayer g) (Facing d)
      forM_ (quickSlot book input) (startCastAt (gamePlayer g) ticksPerInstr)

-- | Where the player settles when movement input stops: per axis, the next
-- tile center along the last motion direction — never behind, so stopping
-- reads as carrying momentum forward, not hopping back. An axis that wasn't
-- moving keeps its containing tile's center. A wall-ward walk already stops
-- on the last open tile's center ('moveAndCollideCentered'), which is a
-- fixed point of the forward snap, so a stop against a wall stays put.
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
  tm <- curMap g
  cmapM_ $ \(Enemy kind cd, Position p, e) -> case kind of
    Dummy -> pure ()
    Chaser -> do
      when (quadrance (pp - p) <= chaserAggro * chaserAggro) $ do
        set e (Facing (signum <$> (pp - p)))
        set e (Position (moveAndCollide tm bodyHalf p (velToward enemySpeed p pp)))
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
          torches <- unlitTorchTiles
          let wv =
                WorldView
                  { wvCaster = pos,
                    wvFacing = facing,
                    wvMana = mana - 1,
                    wvFoes = [(unEntity fe, p) | (fe, p) <- foes],
                    wvTorches = torches
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

-- | Tile coordinates of every unlit torch: what 'UnlitTorch' can select.
-- Rebuilt per instruction (like 'foeSnapshot'), which is what lets each
-- iteration of a kindle loop find the next dark torch.
unlitTorchTiles :: System' [V2 Int]
unlitTorchTiles =
  cfold (\acc (Torch lit, Position p) -> if lit == 0 then tileOf p : acc else acc) []

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
    -- A torch on the tile catches instead of the ground: torches burn on
    -- their own clock ('torchLitTicks') and are what the dungeon door
    -- counts.
    torches <- cfold (\acc (Torch lit, Position p, e) -> if tileOf p == txy then (e, lit) : acc else acc) []
    case torches of
      (e, wasLit) : _ -> do
        set e (Torch torchLitTicks)
        when (wasLit == 0) $ do
          (litN, total) <- cfold (\(l, t) (Torch n) -> (if n > 0 then l + 1 else l, t + 1)) ((0, 0) :: (Int, Int))
          liftIO . hPutStrLn stderr $
            "dungeon: torch lit (" ++ show litN ++ "/" ++ show total ++ ")"
      [] -> do
        burning <- cfold (\acc (Burning t _, e) -> if t == txy then e : acc else acc) []
        case burning of
          e : _ -> set e (Burning txy burnTicks) -- refresh, don't stack
          [] -> newEntity_ (Burning txy burnTicks)
  where
    shove e delta = do
      tm <- curMap g
      mp <- get e
      forM_ (mp :: Maybe Position) $ \(Position p) ->
        set e (Position (moveAndCollide tm bodyHalf p delta))

-- | Fly bolts, expire them on walls and time, land hits on the other side.
tickProjectiles :: Game -> System' ()
tickProjectiles g = do
  tm <- curMap g
  cmapM_ $ \(Projectile vel ttl, Position p, fac :: Faction, e) -> do
    let p' = p + vel
    if ttl <= 0 || boxHitsSolid tm boltHalf p'
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

-- | Burn down torch flames back to unlit — except once the door is open:
-- a solved puzzle stays solved, and its torches keep the hall bright.
tickTorches :: Game -> System' ()
tickTorches g = do
  lvl <- liftIO $ readIORef (gameLevel g)
  unless (lvDoorOpen lvl) $
    cmapM_ $ \(Torch lit, e) ->
      when (lit > 0) $ set e (Torch (lit - 1))

-- | The level's own pulse, last in the tick so no other system runs
-- against a half-swapped world: the torch puzzle unlocks the door, the
-- shrine notices the player, and stairs under the player's center tile
-- swap levels.
tickLevel :: Game -> System' ()
tickLevel g = do
  let dg = gameDungeon g
  lvl <- liftIO $ readIORef (gameLevel g)
  when (lvPlace lvl == InDungeon && not (lvDoorOpen lvl)) $ do
    (litN, total) <- cfold (\(l, t) (Torch n) -> (if n > 0 then l + 1 else l, t + 1)) ((0, 0) :: (Int, Int))
    when (total > 0 && litN == total) $ do
      liftIO . writeIORef (gameLevel g) $
        lvl {lvMap = setTile (dgDoor dg) DoorOpen (lvMap lvl), lvDoorOpen = True}
      liftIO $ hPutStrLn stderr "dungeon: all torches burning - the door grinds open"

  lvl' <- liftIO $ readIORef (gameLevel g)
  Position pp <- get (gamePlayer g)
  let ptile = tileOf pp
  when (lvPlace lvl' == InDungeon && not (lvGoalDone lvl') && ptile == dgGoal dg) $ do
    liftIO $ writeIORef (gameLevel g) lvl' {lvGoalDone = True}
    liftIO $ hPutStrLn stderr "dungeon: dungeon complete"
  case tileAt (lvMap lvl') ptile of
    Just StairsDown -> enterDungeon g
    Just StairsUp -> exitDungeon g
    _ -> pure ()

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

-- | Advance every animation clock and note whether its owner moved this
-- tick. Runs after all movement, and moved-ness comes from comparing
-- positions, so keys, the snap glide, and shoves all count the same.
tickAnim :: System' ()
tickAnim = cmap $ \(Anim prev clock _, Position p) ->
  Anim p (clock + 1) (p /= prev)

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
-- is explicit back-to-front: tiles (bases, then oversized decor), player,
-- enemies, bolts, fire, HUD, damage wash. The caller presents the frame
-- (the editor panel and the game-over veil, when applicable, draw on top).
draw :: Gfx -> Terrain -> Game -> System' ()
draw gfx terrain g = do
  clearFrame gfx (Color 0.03 0.03 0.05 1)
  Position p <- get (gamePlayer g)
  lvl <- liftIO $ readIORef (gameLevel g)
  let tm = lvMap lvl
      inDungeon = lvPlace lvl == InDungeon
      cam = clampCamera tm p
      sprite txy size off (SpriteDraw tex srcPos srcSize tint) =
        drawWorldSprite gfx cam (tileCenter txy + off) size tex srcPos srcSize tint
      castFrac cs = fromIntegral (castSpent cs) / fromIntegral (max 1 (castSize cs)) :: Float
  -- Only the tiles the camera can see: a generated map is 160x160, and
  -- repainting all of it every frame would dwarf everything else drawn.
  let V2 camX camY = cam
      tx0 = max 0 ((camX - windowW `div` 2) `div` tileSize)
      tx1 = min (mapWidth tm - 1) ((camX + windowW `div` 2) `div` tileSize)
      ty0 = max 0 ((camY - windowH `div` 2) `div` tileSize)
      ty1 = min (mapHeight tm - 1) ((camY + windowH `div` 2) `div` tileSize)
  forM_ [V2 tx ty | ty <- [ty0 .. ty1], tx <- [tx0 .. tx1]] $ \txy ->
    forM_ (tileAt tm txy) $ \t ->
      mapM_ (sprite txy (V2 tileSize tileSize) (V2 0 0)) (tileSprites terrain inDungeon tm txy t)
  -- Oversized decor (the dungeon door arch, the shrine circle) goes over
  -- the painted bases, far rows first, cull widened a tile for overhang.
  forM_ [V2 tx ty | ty <- reverse [max 0 (ty0 - 1) .. min (mapHeight tm - 1) (ty1 + 1)], tx <- [max 0 (tx0 - 1) .. min (mapWidth tm - 1) (tx1 + 1)]] $ \txy ->
    forM_ (tileAt tm txy) $ \t ->
      forM_ (tileDecor terrain inDungeon txy t) $ \(sd, size, off) ->
        sprite txy size off sd

  -- The player: the heroes pack Sorcerer over a drop shadow — walk cycle,
  -- idle bob, the cast pose while channeling, the death sprawl under the
  -- game-over veil. Texture color-mod only darkens, so the hurt flash is a
  -- red tint (not the rects' white); i-frames blink translucent. The
  -- sprite's 96x96 cell is mostly margin: the +4 anchor puts the art's
  -- feet on the bottom of the 24x24 collision body.
  mFlash <- get (gamePlayer g)
  mInv <- get (gamePlayer g)
  mChant <- get (gamePlayer g)
  Facing pface <- get (gamePlayer g)
  Health php _ <- get (gamePlayer g)
  Anim _ pclock pmoving <- get (gamePlayer g)
  let playerTint
        | Just (HitFlash _) <- mFlash :: Maybe HitFlash = Just (Color 1 0.35 0.35 1)
        | Just (Invuln n) <- mInv :: Maybe Invuln, odd (n `div` 4) = Just (Color 1 1 1 0.5)
        | otherwise = Nothing
      chanting = isJust (mChant :: Maybe Casting)
      SpriteDraw shTex shSrc shSize shTint = sorcererShadow terrain
      SpriteDraw pTex pSrc pSize pTint =
        sorcererSprite terrain pface (php <= 0) chanting pmoving pclock playerTint
  drawWorldSprite gfx cam (p + V2 0 (-12)) shSize shTex shSrc shSize shTint
  drawWorldSprite gfx cam (p + V2 0 4) pSize pTex pSrc pSize pTint

  -- Enemies (dummies included) as asset_pack monsters over the same drop
  -- shadow as the player: Skeleton chasers, Cultist hexers, Mushy dummies,
  -- the color variant hashed off the entity id. Hurt flashes the red tint
  -- (like the player — color-mod can't whiten). Hurt-only HP bars and,
  -- while one is channeling, the gold cast bar that telegraphs the
  -- interrupt window.
  let SpriteDraw mshTex mshSrc mshSize mshTint = sorcererShadow terrain
  cmapM_ $ \(Enemy kind _, Health cur maxHp, Position ep, Facing eface, Anim _ eclock emoving, e) -> do
    mf <- get e
    let tint = case mf :: Maybe HitFlash of
          Just _ -> Just (Color 1 0.35 0.35 1)
          Nothing -> Nothing
        Entity eid = e
        SpriteDraw eTex eSrc eSize eTint = case kind of
          Dummy -> mushySprite terrain eid eclock tint
          Chaser -> skeletonSprite terrain eid eface emoving eclock tint
          Hexer -> cultistSprite terrain eid eface emoving eclock tint
    drawWorldSprite gfx cam (ep + V2 0 (-10)) mshSize mshTex mshSrc mshSize mshTint
    drawWorldSprite gfx cam (ep + V2 0 4) eSize eTex eSrc eSize eTint
    when (cur < maxHp) $
      drawWorldBar gfx cam (ep + V2 0 20) (fromIntegral cur / fromIntegral maxHp) (Color 0.85 0.2 0.2 1)
    mc <- get e
    forM_ (mc :: Maybe Casting) $ \(Casting cs) ->
      drawWorldBar gfx cam (ep + V2 0 27) (castFrac cs) (Color 0.95 0.8 0.3 1)

  -- Torches: cold floor torch when unlit, the burning frames (door-counting
  -- state, not mere ground fire) once kindled.
  cmapM_ $ \(Torch lit, Position tp) -> do
    let SpriteDraw tex srcPos srcSize tint = torchSprite terrain lit
    drawWorldSprite gfx cam tp (V2 tileSize tileSize) tex srcPos srcSize tint

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
