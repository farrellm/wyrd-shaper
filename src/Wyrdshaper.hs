-- | WyrdShaper — M0 skeleton: a window with a bouncing quad driven by a
-- fixed-timestep ECS system.
module Wyrdshaper (run) where

import Wyrdshaper.Engine
import Wyrdshaper.Loop
import Prelude hiding (lookup)

windowWidth', windowHeight' :: Int
windowWidth' = 800
windowHeight' = 600

quadSize :: Float
quadSize = 64

-- | Velocity in pixels per tick.
newtype Velocity = Velocity (V2 Int)

instance (Monad m) => Component m Velocity

run :: IO ()
run = runAccess_ $ do
  winE <-
    spawn $
      bundle
        Window
          { windowTitle = "WyrdShaper",
            windowWidth = windowWidth',
            windowHeight = windowHeight'
          }
  quadE <-
    spawnColoredRect
      winE
      (Rectangle quadSize quadSize)
      (Color 0.9 0.4 0.1 1)
      (V2 (windowWidth' `div` 2) (windowHeight' `div` 2))
  insert quadE $ bundle (Velocity (V2 3 2))

  runLoop tick $ do
    res <- lookup winE
    pure $ case res of
      Just keys -> keyJustPressed Key'Escape keys
      Nothing -> False

-- | Advance every entity with a 'Velocity' by one tick, bouncing off the
-- window edges.
tick :: (Monad m) => Access m ()
tick = do
  moving <- system . readQuery $ (,) <$> entity <*> query
  mapM_ step moving
  where
    step (e, Velocity v@(V2 vx vy)) = do
      mt <- lookup e
      case mt of
        Just (t :: Transform2D) -> do
          let half = round (quadSize / 2)
              V2 x y = transformTranslation t + v
              (x', vx') = bounce x half (windowWidth' - half) vx
              (y', vy') = bounce y half (windowHeight' - half) vy
          insert e $ bundle (t {transformTranslation = V2 x' y'})
          insert e $ bundle (Velocity (V2 vx' vy'))
        Nothing -> pure ()
    bounce p lo hi v
      | p < lo = (lo, abs v)
      | p > hi = (hi, negate (abs v))
      | otherwise = (p, v)
