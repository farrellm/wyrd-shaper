-- | Fixed-timestep main loop.
--
-- Simulation advances in whole ticks at 'tickRate' regardless of frame rate;
-- rendering happens once per frame. Everything gameplay-visible (movement,
-- the spell VM's per-tick instruction budget) hangs off the tick, never off
-- the frame.
module Wyrdshaper.Loop
  ( tickRate,
    runLoop,
  )
where

import Control.Monad (replicateM_, unless)
import Control.Monad.IO.Class (MonadIO)
import Wyrdshaper.Engine (Input, now, pollInput)

-- | Simulation ticks per second.
tickRate :: Double
tickRate = 60

-- | Largest frame delta we will simulate; longer stalls drop time instead of
-- spiraling (each tick would otherwise make the next frame even later).
maxFrameDelta :: Double
maxFrameDelta = 0.25

-- | Run the frame loop with a fixed-timestep @tick@, drawing each frame,
-- until @shouldQuit@ answers True. Input is polled once per frame; every
-- tick of that frame sees the same snapshot.
--
-- UI\/mode input must be handled in @frame@, never in @tick@: a frame can
-- run zero or two ticks (vsync and the tick clock free-run against each
-- other), so per-tick edge handling would drop or double-fire taps.
-- @shouldQuit@ is answered before @frame@ each frame, so a key @frame@
-- consumes (e.g. Escape closing an editor) cannot also quit.
runLoop ::
  (MonadIO m) =>
  -- | Handle a frame's UI\/mode input; runs once per frame, before ticks.
  (Input -> m ()) ->
  -- | Advance the simulation by one tick.
  (Input -> m ()) ->
  -- | Draw one frame (must end by presenting it).
  m () ->
  -- | Quit? Checked once per frame, before anything else.
  (Input -> m Bool) ->
  m ()
runLoop frame tick draw shouldQuit = go 0 =<< now
  where
    go acc lastT = do
      input <- pollInput
      q <- shouldQuit input
      unless q $ do
        frame input
        t <- now
        let acc' = min maxFrameDelta (acc + (t - lastT))
            steps = floor (acc' * tickRate) :: Int
        replicateM_ steps (tick input)
        draw
        go (acc' - fromIntegral steps / tickRate) t
