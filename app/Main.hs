module Main (main) where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
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

main :: IO ()
main = runAccess_ $ do
  windowEntity <-
    spawn $
      bundle
        Window
          { windowTitle = "WyrdShaper -- M1 Player & World",
            windowWidth = windowW,
            windowHeight = windowH
          }

  worldEntity <- spawn $ bundle (transform2d :: Transform2D)
  spawnTiles worldEntity

  playerEntity <-
    spawn $
      bundle (Rectangle 24 24)
        <> bundle (transform2d {transformTranslation = playerStartPx} :: Transform2D)
        <> bundle (color 0.9 0.2 0.2 1)
        <> bundle (Parent worldEntity)

  runAccessGLFW $ do
    mKeys <- lookup @_ @Keys windowEntity
    let keys = maybe mempty id mKeys

    mTransform <- lookup @_ @Transform2D playerEntity
    case mTransform of
      Just t -> do
        let center' = resolveMove (transformTranslation t) (movementDelta keys)
        insert playerEntity . bundle $ t {transformTranslation = center'}
        insert worldEntity . bundle $
          (transform2d {transformTranslation = cameraOffset center'} :: Transform2D)
      Nothing -> return ()

    render
    liftIO $ threadDelay tickMicros

    case mKeys of
      Just k
        | keyJustPressed Key'Escape k ->
            liftIO (putStrLn "Escape pressed, exiting..." >> hFlush stdout) >> return True
      _ -> return False
