-- | WyrdShaper — M2: the spell VM running as an ECS system. Quick slots
-- 1\/2\/3 channel hardcoded spells over several ticks; bolts fly, dummies
-- take damage and shoves, tiles catch fire, and the HUD shows mana (blue)
-- and cast progress (gold).
module Wyrdshaper (run) where

import Apecs
import Control.Monad (forM_, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Linear (V2 (..))
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Wyrdshaper.Editor
import Wyrdshaper.Engine
import Wyrdshaper.Glyph (compile, cycleField, modifyAt)
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

-- | Collision box half-extents; the player is a 24x24 box in 32px tiles.
playerHalf :: V2 Int
playerHalf = V2 12 12

dummyHalf :: V2 Int
dummyHalf = V2 12 12

boltHalf :: V2 Int
boltHalf = V2 5 5

-- * Shell state

-- | What the player is doing at the meta level. While 'Editing', the
-- simulation is frozen: no ticks run.
data Mode = Playing | Editing EditorState

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
    let (tm, start) = worldMap
    playerE <- newEntity (Position start, Mana manaMax 0, Facing (V2 0 (-1)))
    let game = Game {gameMap = tm, gamePlayer = playerE}

    -- Target dummies on open tiles of the starting room.
    forM_ [V2 18 15, V2 10 13, V2 20 17] $ \txy ->
      newEntity_ (Position (tileCenter txy), DummyHP dummyMaxHP)

    shellRef <- liftIO $ newIORef (Shell Playing book)

    -- Headless smoke test: WYRD_DEMO=1 drives the game under Xvfb where no
    -- keyboard exists: it casts the quick slots on a schedule, then runs a
    -- whole editor pass — open a slot, edit it with glyph ops, commit
    -- through the same path as the Return key, and cast the result.
    -- Scheduled per frame (not per tick): ticks freeze while editing.
    demoFrame <- do
      mDemo <- liftIO $ lookupEnv "WYRD_DEMO"
      case mDemo of
        Nothing -> pure (pure ())
        Just _ -> do
          frameRef <- liftIO $ newIORef (0 :: Int)
          let castSlot k = do
                sh <- liftIO $ readIORef shellRef
                startCast game (slotSpell k (shBook sh))
              openDemoEditor = liftIO $ do
                sh <- liftIO $ readIORef shellRef
                -- volley, with its count bumped 3 -> 4 by a field edit
                let st = openEditor 1 (shBook sh)
                    buf' = modifyAt [0] (cycleField [] 0 1) (edBuf st)
                    st' = maybe st (\b -> st {edBuf = b}) buf'
                writeIORef shellRef sh {shMode = Editing st'}
              commitDemoEdit = do
                sh <- liftIO $ readIORef shellRef
                forM_ [st | Editing st <- [shMode sh]] $ \st ->
                  forM_ (compile (edBuf st)) $ \stmt ->
                    liftIO $ commitShell shellRef (edSlot st) stmt
              script =
                [ (30 :: Int, castSlot 1),
                  (210, castSlot 2),
                  (360, castSlot 0),
                  (450, openDemoEditor),
                  (600, commitDemoEdit),
                  (650, castSlot 1)
                ]
          pure $ do
            n <- liftIO $ atomicModifyIORef' frameRef (\n -> (n + 1, n))
            forM_ (find ((== n) . fst) script) snd

    runLoop
      (\input -> demoFrame >> frame game shellRef input)
      ( \input -> do
          sh <- liftIO $ readIORef shellRef
          case shMode sh of
            Editing _ -> pure () -- the world holds its breath
            Playing -> tick game (shBook sh) input
      )
      ( do
          draw gfx game
          sh <- liftIO $ readIORef shellRef
          forM_ [st | Editing st <- [shMode sh]] (drawEditor gfx)
          presentFrame gfx
      )
      ( \input -> do
          sh <- liftIO $ readIORef shellRef
          let playing = case shMode sh of Playing -> True; Editing _ -> False
          pure (inputQuit input || (playing && keyTapped ScancodeEscape input))
      )

-- | Once-per-frame UI input: opening, driving, and closing the editor.
-- Runs off the frame, not the tick — see 'Wyrdshaper.Loop.runLoop'.
frame :: Game -> IORef Shell -> Input -> System' ()
frame g shellRef input = do
  sh <- liftIO $ readIORef shellRef
  case shMode sh of
    Playing ->
      when (keyTapped ScancodeE input) $ do
        mCast <- get (gamePlayer g)
        case (mCast :: Maybe Casting) of
          Just _ -> pure () -- no leafing through the book mid-incantation
          Nothing ->
            liftIO $ writeIORef shellRef sh {shMode = Editing (openEditor 0 (shBook sh))}
    Editing st -> case updateEditor input (shBook sh) st of
      EdContinue st' -> liftIO $ writeIORef shellRef sh {shMode = Editing st'}
      EdCancel -> liftIO $ writeIORef shellRef sh {shMode = Playing}
      EdCommit slot stmt -> liftIO $ commitShell shellRef slot stmt

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
  tickInput g book input
  tickCast g
  tickProjectiles g
  tickBurning
  tickRegen

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
      set (gamePlayer g) $
        Position (moveAndCollide (gameMap g) playerHalf p (fmap (* speed) dir))
      forM_ [dir | dir /= V2 0 0] $ \d ->
        set (gamePlayer g) (Facing d)
      forM_ (quickSlot book input) (startCast g)

-- | Begin channeling a spell (no-op guard against re-entry is the caller's
-- job; both entry points check for an existing cast). Refuses anything over
-- the Willpower budget — the editor and loader enforce it too, but nothing
-- may channel past it regardless of where the spell came from.
startCast :: Game -> Stmt -> System' ()
startCast g s
  | spellSize s > willpowerMax =
      liftIO $ hPutStrLn stderr ("cast refused: spell exceeds Willpower " ++ show willpowerMax)
  | otherwise =
      set (gamePlayer g) . Casting $
        CastState
          { castVM = newVM s,
            castCooldown = ticksPerInstr,
            castSpent = 0,
            castSize = spellSize s
          }

-- | Advance a channeling spell: every 'ticksPerInstr' ticks, charge one
-- mana, run one VM instruction against a fresh world snapshot, and apply
-- its effects. Fizzles drop the cast (backlash damage arrives in M4).
tickCast :: Game -> System' ()
tickCast g = do
  mCast <- get (gamePlayer g)
  forM_ (mCast :: Maybe Casting) $ \(Casting cs) ->
    if castCooldown cs > 1
      then set (gamePlayer g) (Casting cs {castCooldown = castCooldown cs - 1})
      else do
        Mana mana clock <- get (gamePlayer g)
        if mana <= 0
          then fizzle g OutOfMana
          else do
            set (gamePlayer g) (Mana (mana - 1) clock)
            foes <- dummySnapshot
            Position pos <- get (gamePlayer g)
            Facing facing <- get (gamePlayer g)
            let wv =
                  WorldView
                    { wvCaster = pos,
                      wvFacing = facing,
                      wvMana = mana - 1,
                      wvFoes = [(unEntity e, p) | (e, p) <- foes]
                    }
            case step wv (castVM cs) of
              Fizzle err -> fizzle g err
              Done effs -> do
                mapM_ (applyEffect g foes) effs
                destroy (gamePlayer g) (Proxy @Casting)
              Continue vm' effs -> do
                mapM_ (applyEffect g foes) effs
                set (gamePlayer g) . Casting $
                  cs
                    { castVM = vm',
                      castCooldown = ticksPerInstr,
                      castSpent = castSpent cs + 1
                    }

-- | A collapsed cast: report it and drop the spell. Mana already spoken is
-- gone; the backlash bite lands in M4.
fizzle :: Game -> CastError -> System' ()
fizzle g err = do
  liftIO $ hPutStrLn stderr ("cast fizzled: " ++ show err)
  destroy (gamePlayer g) (Proxy @Casting)

-- | Live dummies with their positions.
dummySnapshot :: System' [(Entity, V2 Int)]
dummySnapshot = cfold (\acc (DummyHP _, Position p, e) -> (e, p) : acc) []

applyEffect :: Game -> [(Entity, V2 Int)] -> Effect -> System' ()
applyEffect g foes eff = case eff of
  SpawnBolt from vel -> newEntity_ (Position from, Projectile vel boltTTL)
  PushEff target delta -> case target of
    TSelf -> shove (gamePlayer g) playerHalf delta
    TFoe fid -> forM_ (find ((== fid) . unEntity . fst) foes) $ \(e, _) -> shove e dummyHalf delta
    TTile _ -> pure () -- the ground declines to move
  KindleEff txy -> do
    burning <- cfold (\acc (Burning t _, e) -> if t == txy then e : acc else acc) []
    case burning of
      e : _ -> set e (Burning txy burnTicks) -- refresh, don't stack
      [] -> newEntity_ (Burning txy burnTicks)
  where
    shove e half delta = do
      mp <- get e
      forM_ (mp :: Maybe Position) $ \(Position p) ->
        set e (Position (moveAndCollide (gameMap g) half p delta))

-- | Fly bolts, expire them on walls and time, land hits on dummies.
tickProjectiles :: Game -> System' ()
tickProjectiles g =
  cmapM_ $ \(Projectile vel ttl, Position p, e) -> do
    let p' = p + vel
    if ttl <= 0 || boxHitsSolid (gameMap g) boltHalf p'
      then destroyEntity e
      else do
        ds <- dummySnapshot
        let V2 ox oy = boltHalf + dummyHalf
            hit (_, V2 dx dy) =
              let V2 px py = p'
               in abs (px - dx) < ox && abs (py - dy) < oy
        case find hit ds of
          Just (de, _) -> do
            damageDummy de
            destroyEntity e
          Nothing -> set e (Position p', Projectile vel (ttl - 1))

damageDummy :: Entity -> System' ()
damageDummy e = do
  mhp <- get e
  forM_ (mhp :: Maybe DummyHP) $ \(DummyHP hp) ->
    if hp <= boltDamage
      then destroyEntity e
      else set e (DummyHP (hp - boltDamage))

-- | Burn down fires; expired ones vanish.
tickBurning :: System' ()
tickBurning = cmapM_ $ \(Burning txy left, e) ->
  if left <= 1
    then destroyEntity e
    else set e (Burning txy (left - 1))

-- | One mana back every 'manaRegenTicks' ticks, up to 'manaMax'.
tickRegen :: System' ()
tickRegen = cmap $ \(Mana cur clock) ->
  if clock + 1 >= manaRegenTicks
    then Mana (min manaMax (cur + 1)) 0
    else Mana cur (clock + 1)

-- * Drawing

-- | Render with the camera on the player, clamped to the map edges; HUD
-- bars for mana and, while channeling, cast progress. Draw order is
-- explicit back-to-front: tiles, player, dummies, bolts, fire, HUD. The
-- caller presents the frame (the editor panel, if open, draws on top).
draw :: Gfx -> Game -> System' ()
draw gfx g = do
  clearFrame gfx (Color 0.03 0.03 0.05 1)
  Position p <- get (gamePlayer g)
  let cam = clampCamera (gameMap g) p
      tileColor t = case t of
        Floor -> Color 0.16 0.22 0.14 1
        Wall -> Color 0.46 0.43 0.38 1
        Water -> Color 0.13 0.25 0.48 1
  forM_ (tiles (gameMap g)) $ \(txy, t) ->
    fillWorldRect gfx cam (tileCenter txy) (V2 tileSize tileSize) (tileColor t)
  fillWorldRect gfx cam p (V2 24 24) (Color 0.9 0.4 0.1 1)
  cmapM_ $ \(DummyHP _, Position dp) ->
    fillWorldRect gfx cam dp (V2 24 24) (Color 0.62 0.45 0.25 1)
  cmapM_ $ \(Projectile _ _, Position bp) ->
    fillWorldRect gfx cam bp (V2 10 10) (Color 1.0 0.85 0.3 1)
  cmapM_ $ \(Burning txy _) ->
    fillWorldRect gfx cam (tileCenter txy) (V2 28 28) (Color 0.95 0.35 0.1 1)
  Mana cur _ <- get (gamePlayer g)
  drawHudBar gfx 0 (fromIntegral cur / fromIntegral manaMax) (Color 0.25 0.55 0.95 1)
  mCast <- get (gamePlayer g)
  forM_ (mCast :: Maybe Casting) $ \(Casting cs) ->
    drawHudBar
      gfx
      1
      (fromIntegral (castSpent cs) / fromIntegral (max 1 (castSize cs)))
      (Color 0.95 0.8 0.3 1)

clampCamera :: Tilemap -> V2 Int -> V2 Int
clampCamera tm (V2 x y) =
  V2
    (clampAxis (windowW `div` 2) (mapWidth tm * tileSize - windowW `div` 2) x)
    (clampAxis (windowH `div` 2) (mapHeight tm * tileSize - windowH `div` 2) y)
  where
    clampAxis lo hi v
      | lo > hi = (lo + hi) `div` 2 -- map smaller than the window: center it
      | otherwise = max lo (min hi v)
