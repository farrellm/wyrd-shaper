{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import Prelude hiding (lookup)

windowW, windowH :: Int
windowW = 800
windowH = 600

-- | Fixed simulation tick rate; also the units of `Velocity` ("px/tick").
tickHz :: Int
tickHz = 60

tickMicros :: Int
tickMicros = 1_000_000 `div` tickHz

-- | Constant per-tick displacement, in the same (V2 Int) units as
-- Transform2D's translation.
newtype Velocity = Velocity (V2 Int)

instance (Monad m) => Component m Velocity

-- | Advance every entity with both a Velocity and a Transform2D, wrapping
-- at the window edges so the shape stays visible forever.
move :: (Monad m) => Query m Transform2D
move = queryMapWith go query
  where
    go (Velocity v) t = t {transformTranslation = wrap (transformTranslation t + v)}
    wrap (V2 x y) = V2 (x `mod` windowW) (y `mod` windowH)

main :: IO ()
main = runAccess_ $ do
  windowEntity <-
    spawn $
      bundle
        Window
          { windowTitle = "WyrdShaper -- M0 Skeleton",
            windowWidth = windowW,
            windowHeight = windowH
          }

  _ <-
    spawn $
      bundle (Rectangle 60 60)
        <> bundle (transform2d {transformTranslation = V2 100 100} :: Transform2D)
        <> bundle (color 1 0 0 1)
        <> bundle (Velocity (V2 3 2))
        <> bundle (Parent windowEntity)

  runAccessGLFW $ do
    _ <- system $ runQuery move
    render
    liftIO $ threadDelay tickMicros
    mKeys <- lookup @_ @Keys windowEntity
    case mKeys of
      Just keys
        | keyJustPressed Key'Escape keys ->
            liftIO (putStrLn "Escape pressed, exiting..." >> hFlush stdout) >> return True
      _ -> return False
