{-# LANGUAGE TypeApplications #-}

-- | WyrdShaper — M2: the spell VM running as an ECS system. Quick slots
-- 1\/2\/3 channel hardcoded spells over several ticks; bolts fly, dummies
-- take damage and shoves, tiles catch fire, and the HUD shows mana (blue)
-- and cast progress (gold).
module Wyrdshaper (run) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Foldable as F
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Wyrdshaper.Engine
import Wyrdshaper.Loop
import Wyrdshaper.Spell
import Wyrdshaper.Tilemap
import Prelude hiding (lookup)

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

-- * Components

-- | Current mana and the regen clock (ticks since the last point back).
data Mana = Mana !Int !Int

instance (Monad m) => Component m Mana

-- | Last nonzero movement direction (components in -1..1); aims 'TileAhead'
-- and slot-1 bolts.
newtype Facing = Facing (V2 Int)

instance (Monad m) => Component m Facing

-- | The player's channeling slot: @Just@ while casting (which roots
-- movement), @Nothing@ when idle. Always present on the player — aztecs
-- 0.17.1's @remove@ corrupts the entity's archetype (see CLAUDE.md), so
-- idle is a value, not an absent component.
newtype Channeling = Channeling (Maybe CastState)

instance (Monad m) => Component m Channeling

data CastState = CastState
  { castVM :: VM,
    -- | Ticks until the next VM instruction.
    castCooldown :: !Int,
    -- | Instructions executed so far (HUD numerator).
    castSpent :: !Int,
    -- | 'spellSize' of the program (HUD denominator).
    castSize :: !Int
  }

-- | A bolt in flight: velocity (px\/tick) and remaining ticks to live.
data Projectile = Projectile !(V2 Int) !Int

instance (Monad m) => Component m Projectile

-- | A target dummy's remaining hit points.
newtype DummyHP = DummyHP Int

instance (Monad m) => Component m DummyHP

-- | A fire overlay: the tile alight and ticks of burn left.
data Burning = Burning !(V2 Int) !Int

instance (Monad m) => Component m Burning

-- * Quick-slot spells (hardcoded until the M3 editor)

-- | Slot 1: a bolt at the tile you face.
fireboltSpell :: Stmt
fireboltSpell = Invoke Bolt [Select TileAhead]

-- | Slot 2: three bolts at the nearest dummy, re-aimed each iteration,
-- launching ticks apart.
volleySpell :: Stmt
volleySpell = Repeat (Lit (VNum 3)) (Invoke Bolt [Select NearestFoe])

-- | Slot 3: bind the nearest dummy, shove it if mana allows, and kindle the
-- tile ahead — let\/seq\/if in one spell.
brandRepelSpell :: Stmt
brandRepelSpell =
  Let "t" (Select NearestFoe) $
    Seq
      [ If
          (BinOp Gt ManaLeft (Lit (VNum 2)))
          (Invoke Push [Var "t"])
          (Seq []),
        Invoke Kindle [Select TileAhead]
      ]

quickSlot :: Keys -> Maybe Stmt
quickSlot keys
  | pressed Key'1 = Just fireboltSpell
  | pressed Key'2 = Just volleySpell
  | pressed Key'3 = Just brandRepelSpell
  | otherwise = Nothing
  where
    pressed k = keyJustPressed k keys

-- * Setup

-- | Everything the systems need each tick: the map and the entities and
-- shared meshes\/materials created at setup.
data Game = Game
  { gameMap :: Tilemap,
    gameWin :: EntityID,
    gamePlayer :: EntityID,
    gameBoltMesh :: EntityID,
    gameBoltMat :: EntityID,
    gameFireMesh :: EntityID,
    gameFireMat :: EntityID,
    gameDummyMesh :: EntityID,
    gameDummyMat :: EntityID
  }

run :: IO ()
run = runAccess_ $ do
  winE <-
    spawn $
      bundle
        Window
          { windowTitle = "WyrdShaper",
            windowWidth = windowW,
            windowHeight = windowH
          }
  enableVSync winE

  let (tm, start) = worldMap
  spawnTilemap winE tm

  playerE <-
    spawnColoredRect
      winE
      (Rectangle 24 24)
      (Color 0.9 0.4 0.1 1)
      start
  insert playerE $ bundle (Mana manaMax 0)
  insert playerE $ bundle (Facing (V2 0 (-1)))
  insert playerE $ bundle (Channeling Nothing)

  -- Shared meshes/materials, created after the player so their render
  -- groups (ordered by entity id) draw above the tiles and the player.
  dummyMesh <- sharedRectMesh winE (Rectangle 24 24)
  dummyMat <- colorMaterial (Color 0.62 0.45 0.25 1)
  boltMesh <- sharedRectMesh winE (Rectangle 10 10)
  boltMat <- colorMaterial (Color 1.0 0.85 0.3 1)
  fireMesh <- sharedRectMesh winE (Rectangle 28 28)
  fireMat <- colorMaterial (Color 0.95 0.35 0.1 1)

  let game =
        Game
          { gameMap = tm,
            gameWin = winE,
            gamePlayer = playerE,
            gameBoltMesh = boltMesh,
            gameBoltMat = boltMat,
            gameFireMesh = fireMesh,
            gameFireMat = fireMat,
            gameDummyMesh = dummyMesh,
            gameDummyMat = dummyMat
          }

  -- Target dummies on open tiles of the starting room.
  spawnDummies game [V2 18 15, V2 10 13, V2 20 17]

  -- Headless smoke test: WYRD_DEMO=1 auto-casts the quick slots on a
  -- schedule, for driving the game under Xvfb where no keyboard exists.
  demoTick <- do
    mDemo <- liftIO $ lookupEnv "WYRD_DEMO"
    case mDemo of
      Nothing -> pure (pure ())
      Just _ -> do
        tickRef <- liftIO $ newIORef (0 :: Int)
        let script = [(30, volleySpell), (210, brandRepelSpell), (360, fireboltSpell)]
        pure $ do
          n <- liftIO $ atomicModifyIORef' tickRef (\n -> (n + 1, n))
          forM_ (find ((== n) . fst) script) $ \(_, s) -> startCast game s

  runLoop (demoTick >> tick game) (draw game) $ do
    mKeys <- lookup winE
    pure $ maybe False (keyJustPressed Key'Escape) mKeys

-- | One shared 32x32 quad mesh, one material per tile kind, one lightweight
-- entity (just a transform) per tile.
spawnTilemap :: (MonadIO m) => EntityID -> Tilemap -> Access m ()
spawnTilemap winE tm = do
  meshE <- sharedRectMesh winE (Rectangle (fromIntegral tileSize) (fromIntegral tileSize))
  floorMat <- colorMaterial (Color 0.16 0.22 0.14 1)
  wallMat <- colorMaterial (Color 0.46 0.43 0.38 1)
  waterMat <- colorMaterial (Color 0.13 0.25 0.48 1)
  placed <- forM (tiles tm) $ \(txy, t) -> do
    e <- spawn $ bundle (transform2d {transformTranslation = tileCenter txy} :: Transform2D)
    pure (t, e)
  let ofKind k = [e | (t, e) <- placed, t == k]
  registerInstances meshE floorMat (ofKind Floor)
  registerInstances meshE wallMat (ofKind Wall)
  registerInstances meshE waterMat (ofKind Water)

spawnDummies :: (MonadIO m) => Game -> [V2 Int] -> Access m ()
spawnDummies g txys = do
  es <- forM txys $ \txy ->
    spawn $
      bundle (transform2d {transformTranslation = tileCenter txy} :: Transform2D)
        <> bundle (DummyHP dummyMaxHP)
  registerInstances (gameDummyMesh g) (gameDummyMat g) es

-- * Tick systems

tick :: (MonadIO m) => Game -> Access m ()
tick g = do
  tickInput g
  tickCast g
  tickProjectiles g
  tickBurning g
  tickRegen g

-- | Movement and cast start. Channeling roots the player: no movement, no
-- new casts.
tickInput :: (MonadIO m) => Game -> Access m ()
tickInput g = do
  mCast <- lookup (gamePlayer g)
  case (mCast :: Maybe Channeling) of
    Just (Channeling (Just _)) -> pure ()
    _ -> do
      keys <- fromMaybe mempty <$> lookup (gameWin g)
      let axis neg neg' pos pos' =
            (if keyPressed pos keys || keyPressed pos' keys then 1 else 0)
              - (if keyPressed neg keys || keyPressed neg' keys then 1 else 0)
          dir = V2 (axis Key'A Key'Left Key'D Key'Right) (axis Key'S Key'Down Key'W Key'Up)
          -- crude diagonal compensation: 2px on both axes vs 3px on one
          speed = case dir of V2 x y | x /= 0 && y /= 0 -> 2; _ -> playerSpeed
      mt <- lookup (gamePlayer g)
      forM_ (mt :: Maybe Transform2D) $ \t -> do
        let p = moveAndCollide (gameMap g) playerHalf (transformTranslation t) (fmap (* speed) dir)
        insert (gamePlayer g) $ bundle (t {transformTranslation = p})
      forM_ [dir | dir /= V2 0 0] $ \d ->
        insert (gamePlayer g) $ bundle (Facing d)
      forM_ (quickSlot keys) (startCast g)

-- | Begin channeling a spell (no-op guard against re-entry is the caller's
-- job; both entry points check for an existing 'CastState').
startCast :: (Monad m) => Game -> Stmt -> Access m ()
startCast g s =
  insert (gamePlayer g) . bundle . Channeling . Just $
    CastState
      { castVM = newVM s,
        castCooldown = ticksPerInstr,
        castSpent = 0,
        castSize = spellSize s
      }

-- | Advance a channeling spell: every 'ticksPerInstr' ticks, charge one
-- mana, run one VM instruction against a fresh world snapshot, and apply
-- its effects. Fizzles drop the cast (backlash damage arrives in M4).
tickCast :: (MonadIO m) => Game -> Access m ()
tickCast g = do
  mCast <- lookup (gamePlayer g)
  forM_ [cs | Channeling (Just cs) <- F.toList (mCast :: Maybe Channeling)] $ \cs ->
    if castCooldown cs > 1
      then
        insert (gamePlayer g) $
          bundle (Channeling (Just cs {castCooldown = castCooldown cs - 1}))
      else do
        Mana mana clock <- fromMaybe (Mana 0 0) <$> lookup (gamePlayer g)
        if mana <= 0
          then fizzle g OutOfMana
          else do
            insert (gamePlayer g) $ bundle (Mana (mana - 1) clock)
            foes <- dummySnapshot
            mt <- lookup (gamePlayer g)
            mf <- lookup (gamePlayer g)
            let pos = maybe (V2 0 0) transformTranslation (mt :: Maybe Transform2D)
                Facing facing = fromMaybe (Facing (V2 0 (-1))) mf
                wv =
                  WorldView
                    { wvCaster = pos,
                      wvFacing = facing,
                      wvMana = mana - 1,
                      wvFoes = [(entityKey e, p) | (e, p) <- foes]
                    }
            case step wv (castVM cs) of
              Fizzle err -> fizzle g err
              Done effs -> do
                mapM_ (applyEffect g foes) effs
                insert (gamePlayer g) $ bundle (Channeling Nothing)
              Continue vm' effs -> do
                mapM_ (applyEffect g foes) effs
                insert (gamePlayer g) . bundle . Channeling . Just $
                  cs
                    { castVM = vm',
                      castCooldown = ticksPerInstr,
                      castSpent = castSpent cs + 1
                    }

-- | A collapsed cast: report it and drop the spell. Mana already spoken is
-- gone; the backlash bite lands in M4.
fizzle :: (MonadIO m) => Game -> CastError -> Access m ()
fizzle g err = do
  liftIO $ hPutStrLn stderr ("cast fizzled: " ++ show err)
  insert (gamePlayer g) $ bundle (Channeling Nothing)

-- | Live dummies with their positions.
dummySnapshot :: (Monad m) => Access m [(EntityID, V2 Int)]
dummySnapshot = do
  ds <-
    system . readQuery $
      (,,) <$> entity <*> query @_ @DummyHP <*> query @_ @Transform2D
  pure [(e, transformTranslation t) | (e, DummyHP _, t) <- F.toList ds]

applyEffect :: (MonadIO m) => Game -> [(EntityID, V2 Int)] -> Effect -> Access m ()
applyEffect g foes eff = case eff of
  SpawnBolt from vel -> do
    -- Spawn with the complete bundle: spawn-then-insert would move the
    -- entity between archetypes, which aztecs misaligns unless the mover
    -- has the highest id (see CLAUDE.md).
    e <-
      spawn $
        bundle (transform2d {transformTranslation = from} :: Transform2D)
          <> bundle (Projectile vel boltTTL)
    registerInstances (gameBoltMesh g) (gameBoltMat g) [e]
  PushEff target delta -> case target of
    TSelf -> shove (gamePlayer g) playerHalf delta
    TFoe fid -> forM_ (find ((== fid) . entityKey . fst) foes) $ \(e, _) -> shove e dummyHalf delta
    TTile _ -> pure () -- the ground declines to move
  KindleEff txy -> do
    burns <- system . readQuery $ (,) <$> entity <*> query @_ @Burning
    case [e | (e, Burning t _) <- F.toList burns, t == txy] of
      e : _ -> insert e $ bundle (Burning txy burnTicks) -- refresh, don't stack
      [] -> do
        e <-
          spawn $
            bundle (transform2d {transformTranslation = tileCenter txy} :: Transform2D)
              <> bundle (Burning txy burnTicks)
        registerInstances (gameFireMesh g) (gameFireMat g) [e]
  where
    shove e half delta = do
      mt <- lookup e
      forM_ (mt :: Maybe Transform2D) $ \t -> do
        let p = moveAndCollide (gameMap g) half (transformTranslation t) delta
        insert e $ bundle (t {transformTranslation = p})

-- | Fly bolts, expire them on walls and time, land hits on dummies.
tickProjectiles :: (MonadIO m) => Game -> Access m ()
tickProjectiles g = do
  ps <-
    system . readQuery $
      (,,) <$> entity <*> query @_ @Projectile <*> query @_ @Transform2D
  forM_ (F.toList ps) $ \(e, Projectile vel ttl, t) -> do
    let p' = transformTranslation t + vel
    if ttl <= 0 || boxHitsSolid (gameMap g) boltHalf p'
      then despawnBolt e
      else do
        ds <- dummySnapshot
        let V2 ox oy = boltHalf + dummyHalf
            hit (_, V2 dx dy) =
              let V2 px py = p'
               in abs (px - dx) < ox && abs (py - dy) < oy
        case find hit ds of
          Just (de, _) -> do
            damageDummy g de
            despawnBolt e
          Nothing -> do
            insert e $ bundle (t {transformTranslation = p'})
            insert e $ bundle (Projectile vel (ttl - 1))
  where
    despawnBolt e = do
      unregisterInstances (gameBoltMesh g) (gameBoltMat g) [e]
      despawn e

damageDummy :: (MonadIO m) => Game -> EntityID -> Access m ()
damageDummy g e = do
  mhp <- lookup e
  forM_ (mhp :: Maybe DummyHP) $ \(DummyHP hp) ->
    if hp <= boltDamage
      then do
        unregisterInstances (gameDummyMesh g) (gameDummyMat g) [e]
        despawn e
      else insert e $ bundle (DummyHP (hp - boltDamage))

-- | Burn down fires; expired ones vanish.
tickBurning :: (MonadIO m) => Game -> Access m ()
tickBurning g = do
  bs <- system . readQuery $ (,) <$> entity <*> query @_ @Burning
  forM_ (F.toList bs) $ \(e, Burning txy left) ->
    if left <= 1
      then do
        unregisterInstances (gameFireMesh g) (gameFireMat g) [e]
        despawn e
      else insert e $ bundle (Burning txy (left - 1))

-- | One mana back every 'manaRegenTicks' ticks, up to 'manaMax'.
tickRegen :: (Monad m) => Game -> Access m ()
tickRegen g = do
  mm <- lookup (gamePlayer g)
  forM_ (mm :: Maybe Mana) $ \(Mana cur clock) ->
    insert (gamePlayer g) $
      bundle
        ( if clock + 1 >= manaRegenTicks
            then Mana (min manaMax (cur + 1)) 0
            else Mana cur (clock + 1)
        )

-- * Drawing

-- | Render with the camera on the player, clamped to the map edges; HUD
-- bars for mana and, while channeling, cast progress.
draw :: (MonadIO m) => Game -> Access m ()
draw g = do
  mt <- lookup (gamePlayer g)
  mm <- lookup (gamePlayer g)
  mc <- lookup (gamePlayer g)
  let p = maybe (V2 0 0) transformTranslation (mt :: Maybe Transform2D)
      manaFrac = case mm of
        Just (Mana cur _) -> fromIntegral cur / fromIntegral manaMax
        Nothing -> 0
      castBar = case mc of
        Just (Channeling (Just cs)) ->
          [ ( fromIntegral (castSpent cs) / fromIntegral (max 1 (castSize cs)),
              Color 0.95 0.8 0.3 1
            )
          ]
        _ -> []
      bars = (manaFrac, Color 0.25 0.55 0.95 1) : castBar
  renderWithCamera (clampCamera (gameMap g) p) bars

clampCamera :: Tilemap -> V2 Int -> V2 Int
clampCamera tm (V2 x y) =
  V2
    (clampAxis (windowW `div` 2) (mapWidth tm * tileSize - windowW `div` 2) x)
    (clampAxis (windowH `div` 2) (mapHeight tm * tileSize - windowH `div` 2) y)
  where
    clampAxis lo hi v
      | lo > hi = (lo + hi) `div` 2 -- map smaller than the window: center it
      | otherwise = max lo (min hi v)
