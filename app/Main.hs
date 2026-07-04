{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe, listToMaybe)
import Spell
import System.IO (hFlush, stdout)
import Tilemap
import Prelude hiding (lookup)

windowW, windowH :: Int
windowW = 800
windowH = 600

-- | Fixed simulation tick rate.
tickHz :: Int
tickHz = 60

tickMicros :: Int
tickMicros = 1_000_000 `div` tickHz

playerSpeed :: Int
playerSpeed = 3

playerHalfExtent :: V2 Int
playerHalfExtent = V2 12 12

-- | Ticks between spell instructions -- one raw tick/instruction (~17ms at
-- 60Hz) would technically satisfy "multi-tick" but be invisible. This is
-- the number that makes a three-bolt spell actually read as three bolts
-- fired one after another, per CONCEPT.md's "ticks apart".
instrTicks :: Int
instrTicks = 18

-- | Ticks between +1 mana regen (~3/sec at 60Hz). Placeholder, tune later.
manaRegenTicks :: Int
manaRegenTicks = 20

startingManaMax :: Int
startingManaMax = 20

-- | Ticks an impact flash stays alive before despawning.
flashLifetimeTicks :: Int
flashLifetimeTicks = 12

manaBarMaxWidth, manaBarHeight :: Float
manaBarMaxWidth = 200
manaBarHeight = 16

manaBarMargin :: Int
manaBarMargin = 20

movementDelta :: Keys -> V2 Int
movementDelta keys = V2 dx dy
  where
    axis pos neg =
      (if keyPressed pos keys then playerSpeed else 0)
        - (if keyPressed neg keys then playerSpeed else 0)
    dx = axis Key'D Key'A
    dy = axis Key'W Key'S

-- | The four corners of an axis-aligned box, using the last occupied pixel
-- (not the exclusive boundary) on the high side so a box flush against a
-- tile edge doesn't falsely register as overlapping the next tile.
aabbCorners :: V2 Int -> V2 Int -> [V2 Int]
aabbCorners (V2 cx cy) (V2 hw hh) =
  [ V2 (cx - hw) (cy - hh),
    V2 (cx + hw - 1) (cy - hh),
    V2 (cx - hw) (cy + hh - 1),
    V2 (cx + hw - 1) (cy + hh - 1)
  ]

collides :: V2 Int -> Bool
collides center = any (isWallAtPx tileMap) (aabbCorners center playerHalfExtent)

-- | Resolve a proposed move against the tilemap one axis at a time, so the
-- player slides along a wall instead of stopping dead on diagonal input.
resolveMove :: V2 Int -> V2 Int -> V2 Int
resolveMove center (V2 dx dy) =
  let afterX = center + V2 dx 0
      center' = if collides afterX then center else afterX
      afterY = center' + V2 0 dy
   in if collides afterY then center' else afterY

-- | Clamp a camera offset so the window never shows past the map edge on
-- this axis, centering the player otherwise. Falls back to centering the
-- map itself if it's smaller than the window on this axis.
clampAxis :: Int -> Int -> Int -> Int
clampAxis playerPos winSize mapSize
  | mapSize <= winSize = (winSize - mapSize) `div` 2
  | otherwise = max (winSize - mapSize) (min 0 (winSize `div` 2 - playerPos))

cameraOffset :: V2 Int -> V2 Int
cameraOffset (V2 px py) =
  V2 (clampAxis px windowW mapWidthPx) (clampAxis py windowH mapHeightPx)

-- | The player's tile-space facing, as a unit (colDelta, rowTopDelta) step
-- -- default south, i.e. (0, 1), matching 'Tilemap.tileCenterPx's
-- convention that increasing rowTop is south. Drives the 'AheadSel'
-- selector and is only updated while idle (facing freezes while casting).
newtype Facing = Facing (Int, Int) deriving (Eq, Show)

instance (Monad m) => Component m Facing

-- | Mana pool. Regenerates passively every tick via 'stepManaRegen',
-- independent of casting; instructions spend it on execution.
data Mana = Mana
  { manaCurrent :: Int,
    manaMax :: Int,
    manaRegenTimer :: Int
  }
  deriving (Eq, Show)

instance (Monad m) => Component m Mana

-- | The player's channeling state -- a flat program plus a per-instruction
-- windup countdown. Idle is @Casting [] 0@.
data Casting = Casting
  { castingProgram :: Program,
    castingTicksLeft :: Int
  }
  deriving (Eq, Show)

instance (Monad m) => Component m Casting

-- | The boulder's current tile position -- the only prop that actually
-- moves at tile-grid granularity (the torch is static, so it just reads
-- 'Tilemap.torchTile' and never needs this).
newtype TilePos = TilePos (Int, Int) deriving (Eq, Show)

instance (Monad m) => Component m TilePos

-- | Whether the torch has been kindled.
newtype Lit = Lit Bool deriving (Eq, Show)

instance (Monad m) => Component m Lit

-- | Ticks remaining before a transient effect (a bolt impact flash)
-- despawns.
newtype Lifetime = Lifetime Int deriving (Eq, Show)

instance (Monad m) => Component m Lifetime

-- | The fixed, known entities the tick loop threads through -- warranted
-- now that there are more than the two (player, world) M1 tracked as loose
-- closure variables.
data GameEntities = GameEntities
  { windowEntity :: EntityID,
    worldEntity :: EntityID,
    playerEntity :: EntityID,
    boulderEntity :: EntityID,
    torchEntity :: EntityID,
    manaBarFillEntity :: EntityID
  }

-- | 'despawn' does NOT run component lifecycle hooks (verified against the
-- 'Aztecs.ECS.Access'/'Aztecs.ECS.World.Entities' source: unlike
-- 'insert'/'remove'/'spawn', there's no hook invocation in that path at
-- all). Calling it directly on a renderable, parented entity would skip
-- 'Rectangle'/'Material's hooks (VBO + render-group cleanup) and, worse,
-- 'Parent's hook -- which is what detaches the entity from its parent's
-- 'Children' set. Left unremoved, 'Children' only grows, and transform
-- propagation walks the whole set every tick the parent moves. Always
-- 'remove' hook-owning components first.
despawnRenderable :: EntityID -> Access IO ()
despawnRenderable e = do
  _ <- remove @_ @Rectangle e
  _ <- remove @_ @Material e
  _ <- remove @_ @Parent e
  despawn e

-- | Decrement every live 'Lifetime' and despawn whatever hits zero. The one
-- place M2 needs a real dynamic 'Query'/'System' rather than a hand-tracked
-- 'EntityID', since impact flashes are spawned dynamically and unbounded in
-- count -- mirrors the animation-stepping pattern in
-- @aztecs-examples/src/SpriteSheet.hs@.
stepLifetimes :: Access IO ()
stepLifetimes = do
  expired <- system . runQuery $ (,) <$> entity <*> queryMapAccum step
  mapM_ despawnRenderable [e | (e, (True, _)) <- F.toList expired]
  where
    step (Lifetime n) = let n' = n - 1 in (n' <= 0, Lifetime n')

stepManaRegen :: Mana -> Mana
stepManaRegen m
  | manaCurrent m >= manaMax m = m {manaRegenTimer = 0}
  | manaRegenTimer m + 1 >= manaRegenTicks =
      m {manaCurrent = min (manaMax m) (manaCurrent m + 1), manaRegenTimer = 0}
  | otherwise = m {manaRegenTimer = manaRegenTimer m + 1}

-- | Which quick-slot spell (if any) just started this tick.
quickSlotJustPressed :: Keys -> Maybe Program
quickSlotJustPressed keys =
  listToMaybe
    [prog | k <- [Key'1, Key'2, Key'3], keyJustPressed k keys, Just prog <- [quickSlotProgram k]]

-- | Movement/collision/camera-follow, unchanged from M1, plus updating
-- 'Facing' from whatever direction the player actually moved in (left
-- alone -- "last facing", not "current input" -- when there's no movement).
stepMovementAndFacing :: GameEntities -> Keys -> Transform2D -> Access IO ()
stepMovementAndFacing ge keys t = do
  let delta = movementDelta keys
      center' = resolveMove (transformTranslation t) delta
  insert (playerEntity ge) . bundle $ t {transformTranslation = center'}
  insert (worldEntity ge) . bundle $
    (transform2d {transformTranslation = cameraOffset center'} :: Transform2D)
  case tileFacingFromMovement delta of
    Just f -> insert (playerEntity ge) . bundle $ Facing f
    Nothing -> return ()

-- | Spawn a short-lived impact flash at a tile -- the effect that
-- demonstrates "effects land in the world" for 'Bolt', which always hits
-- whatever's ahead regardless of what's there.
spawnImpact :: GameEntities -> (Int, Int) -> Access IO ()
spawnImpact ge tilePos =
  spawn_ $
    bundle (Rectangle (fromIntegral tileSize * 0.6) (fromIntegral tileSize * 0.6))
      <> bundle (transform2d {transformTranslation = tileCenterPx tilePos} :: Transform2D)
      <> bundle (color 1.0 0.55 0.1 1)
      <> bundle (Parent (worldEntity ge))
      <> bundle (Lifetime flashLifetimeTicks)

-- | Apply one instruction's effect. Mana is spent whether or not the
-- effect finds anything to act on -- CONCEPT.md is explicit that cost is
-- "per instruction executed, not per cast".
applyEffect :: GameEntities -> Verb -> Selector -> (Int, Int) -> (Int, Int) -> Access IO ()
applyEffect ge verb sel playerTile facing =
  case verb of
    Bolt -> spawnImpact ge aheadTile
    Push -> do
      mPos <- lookup @_ @TilePos (boulderEntity ge)
      case mPos of
        Just (TilePos p)
          | p == aheadTile,
            not (isWallAtTile tileMap destTile),
            destTile /= torchTile ->
              insert (boulderEntity ge) $
                bundle (TilePos destTile)
                  <> bundle (transform2d {transformTranslation = tileCenterPx destTile} :: Transform2D)
        _ -> liftIO $ putStrLn "push: nothing to push there"
    Kindle
      | aheadTile == torchTile -> do
          mLit <- lookup @_ @Lit (torchEntity ge)
          case mLit of
            Just (Lit True) -> return ()
            _ -> insert (torchEntity ge) $ bundle (Lit True) <> bundle (color 1.0 0.55 0.05 1)
      | otherwise -> liftIO $ putStrLn "kindle: nothing to kindle there"
  where
    aheadTile = resolveSelector sel playerTile facing
    destTile = aheadTile `addTile` facing

-- | Update the mana-bar HUD fill rectangle's width/position to track
-- @manaCurrent / manaMax@ -- makes the mana system observable during manual
-- testing, since there's no text rendering yet to show the number.
stepManaBar :: GameEntities -> Mana -> Access IO ()
stepManaBar ge mana = do
  let frac = fromIntegral (manaCurrent mana) / fromIntegral (manaMax mana) :: Float
      w = max 1 (manaBarMaxWidth * frac)
      x = manaBarMargin + round (w / 2)
      y = windowH - manaBarMargin
  insert (manaBarFillEntity ge) $
    bundle (Rectangle w manaBarHeight)
      <> bundle (transform2d {transformTranslation = V2 x y} :: Transform2D)

tick :: GameEntities -> Access IO Bool
tick ge = do
  stepLifetimes

  mKeys <- lookup @_ @Keys (windowEntity ge)
  let keys = fromMaybe mempty mKeys

  mTransform <- lookup @_ @Transform2D (playerEntity ge)
  mFacing <- lookup @_ @Facing (playerEntity ge)
  mMana <- lookup @_ @Mana (playerEntity ge)
  mCasting <- lookup @_ @Casting (playerEntity ge)

  case (mTransform, mFacing, mMana, mCasting) of
    (Just t, Just (Facing facing), Just mana, Just casting) -> do
      let manaAfterRegen = stepManaRegen mana
          playerTile = pixelToTile (transformTranslation t)

      finalMana <- case castingProgram casting of
        (Cast verb sel : rest)
          | castingTicksLeft casting > 1 -> do
              insert (playerEntity ge) . bundle $
                casting {castingTicksLeft = castingTicksLeft casting - 1}
              return manaAfterRegen
          | manaCurrent manaAfterRegen >= manaCost verb -> do
              applyEffect ge verb sel playerTile facing
              insert (playerEntity ge) . bundle $
                if null rest then Casting [] 0 else Casting rest instrTicks
              return manaAfterRegen {manaCurrent = manaCurrent manaAfterRegen - manaCost verb}
          | otherwise -> do
              liftIO $ putStrLn "spell fizzles: mana exhausted"
              insert (playerEntity ge) . bundle $ Casting [] 0
              return manaAfterRegen
        [] -> case quickSlotJustPressed keys of
          Just prog@(Cast verb0 _ : _)
            | manaCurrent manaAfterRegen >= manaCost verb0 -> do
                insert (playerEntity ge) . bundle $ Casting prog instrTicks
                return manaAfterRegen
          _ -> do
            stepMovementAndFacing ge keys t
            return manaAfterRegen

      insert (playerEntity ge) . bundle $ finalMana
      stepManaBar ge finalMana
    _ -> return ()

  render
  liftIO $ threadDelay tickMicros

  case mKeys of
    Just k
      | keyJustPressed Key'Escape k ->
          liftIO (putStrLn "Escape pressed, exiting..." >> hFlush stdout) >> return True
    _ -> return False

main :: IO ()
main = runAccess_ $ do
  windowEntity' <-
    spawn $
      bundle
        Window
          { windowTitle = "WyrdShaper -- M2 Spell VM core",
            windowWidth = windowW,
            windowHeight = windowH
          }

  worldEntity' <- spawn $ bundle (transform2d :: Transform2D)
  spawnTiles worldEntity'

  playerEntity' <-
    spawn $
      bundle (Rectangle 24 24)
        <> bundle (transform2d {transformTranslation = playerStartPx} :: Transform2D)
        <> bundle (color 0.9 0.2 0.2 1)
        <> bundle (Parent worldEntity')
        <> bundle (Facing (0, 1))
        <> bundle (Mana startingManaMax startingManaMax 0)
        <> bundle (Casting [] 0)

  boulderEntity' <-
    spawn $
      bundle (Rectangle (fromIntegral tileSize * 0.8) (fromIntegral tileSize * 0.8))
        <> bundle (transform2d {transformTranslation = tileCenterPx boulderStartTile} :: Transform2D)
        <> bundle (color 0.5 0.35 0.2 1)
        <> bundle (Parent worldEntity')
        <> bundle (TilePos boulderStartTile)

  torchEntity' <-
    spawn $
      bundle (Rectangle (fromIntegral tileSize * 0.5) (fromIntegral tileSize * 0.5))
        <> bundle (transform2d {transformTranslation = tileCenterPx torchTile} :: Transform2D)
        <> bundle (color 0.4 0.4 0.4 1)
        <> bundle (Parent worldEntity')
        <> bundle (Lit False)

  let manaBarPos =
        V2 (manaBarMargin + round (manaBarMaxWidth / 2)) (windowH - manaBarMargin)

  _ <-
    spawn $
      bundle (Rectangle manaBarMaxWidth manaBarHeight)
        <> bundle (transform2d {transformTranslation = manaBarPos} :: Transform2D)
        <> bundle (color 0.1 0.1 0.1 1)
        <> bundle (Parent windowEntity')

  manaBarFillEntity' <-
    spawn $
      bundle (Rectangle manaBarMaxWidth manaBarHeight)
        <> bundle (transform2d {transformTranslation = manaBarPos} :: Transform2D)
        <> bundle (color 0.2 0.5 0.9 1)
        <> bundle (Parent windowEntity')

  let ge =
        GameEntities
          { windowEntity = windowEntity',
            worldEntity = worldEntity',
            playerEntity = playerEntity',
            boulderEntity = boulderEntity',
            torchEntity = torchEntity',
            manaBarFillEntity = manaBarFillEntity'
          }

  runAccessGLFW $ tick ge
