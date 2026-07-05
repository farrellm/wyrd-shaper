-- | WyrdShaper — M1: hand-authored tilemap, player movement with collision,
-- camera follow.
module Wyrdshaper (run) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Wyrdshaper.Engine
import Wyrdshaper.Loop
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

  runLoop (tick tm winE playerE) (draw tm playerE) $ do
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

-- | Read movement keys, move the player with collision.
tick :: (Monad m) => Tilemap -> EntityID -> EntityID -> Access m ()
tick tm winE playerE = do
  keys <- fromMaybe mempty <$> lookup winE
  let axis neg neg' pos pos' =
        (if keyPressed pos keys || keyPressed pos' keys then 1 else 0)
          - (if keyPressed neg keys || keyPressed neg' keys then 1 else 0)
      dir = V2 (axis Key'A Key'Left Key'D Key'Right) (axis Key'S Key'Down Key'W Key'Up)
      -- crude diagonal compensation: 2px on both axes vs 3px on one
      speed = case dir of V2 x y | x /= 0 && y /= 0 -> 2; _ -> playerSpeed
  mt <- lookup playerE
  case mt of
    Just (t :: Transform2D) -> do
      let p = moveAndCollide tm playerHalf (transformTranslation t) (fmap (* speed) dir)
      insert playerE $ bundle (t {transformTranslation = p})
    Nothing -> pure ()

-- | Render with the camera on the player, clamped to the map edges.
draw :: (MonadIO m) => Tilemap -> EntityID -> Access m ()
draw tm playerE = do
  mt <- lookup playerE
  let p = maybe (V2 0 0) transformTranslation (mt :: Maybe Transform2D)
  renderWithCamera (clampCamera tm p)

clampCamera :: Tilemap -> V2 Int -> V2 Int
clampCamera tm (V2 x y) =
  V2
    (clampAxis (windowW `div` 2) (mapWidth tm * tileSize - windowW `div` 2) x)
    (clampAxis (windowH `div` 2) (mapHeight tm * tileSize - windowH `div` 2) y)
  where
    clampAxis lo hi v
      | lo > hi = (lo + hi) `div` 2 -- map smaller than the window: center it
      | otherwise = max lo (min hi v)
