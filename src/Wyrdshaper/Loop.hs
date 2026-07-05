-- | Fixed-timestep main loop.
--
-- Simulation advances in whole ticks at 'tickRate' regardless of frame rate;
-- rendering happens once per frame. Everything gameplay-visible (movement,
-- and later the spell VM's per-tick instruction budget) hangs off the tick,
-- never off the frame.
module Wyrdshaper.Loop
  ( tickRate,
    runLoop,
  )
where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Graphics.UI.GLFW as GLFW
import Wyrdshaper.Engine
import Prelude hiding (lookup)

-- | Simulation ticks per second.
tickRate :: Double
tickRate = 60

-- | Largest frame delta we will simulate; longer stalls drop time instead of
-- spiraling (each tick would otherwise make the next frame even later).
maxFrameDelta :: Double
maxFrameDelta = 0.25

-- | Run the GLFW frame loop with a fixed-timestep @tick@, rendering each
-- frame, until the window closes or @shouldQuit@ answers True.
runLoop ::
  (MonadIO m) =>
  -- | Advance the simulation by one tick.
  Access m () ->
  -- | Quit? Checked once per frame, after rendering.
  Access m Bool ->
  Access m ()
runLoop tick shouldQuit = do
  accRef <- liftIO $ newIORef (0 :: Double)
  lastRef <- liftIO $ newIORef =<< liftIO now
  runAccessGLFW $ do
    t <- liftIO now
    lastT <- liftIO $ readIORef lastRef
    liftIO $ writeIORef lastRef t
    acc <- liftIO $ readIORef accRef
    let acc' = min maxFrameDelta (acc + (t - lastT))
        steps = floor (acc' * tickRate) :: Int
    liftIO $ writeIORef accRef (acc' - fromIntegral steps / tickRate)
    replicateM_ steps tick
    render
    shouldQuit
  where
    now = fromMaybe 0 <$> GLFW.getTime
