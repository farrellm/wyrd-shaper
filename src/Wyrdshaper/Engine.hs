-- | The SDL2-facing layer: window\/renderer lifecycle, per-frame input
-- snapshots, the camera transform, and immediate-mode rect drawing.
--
-- The only module that imports SDL. It also owns the sole flip from world
-- coordinates (pixels, origin bottom-left, y up) to SDL screen coordinates
-- (origin top-left, y down): 'fillWorldRect' and 'drawHudBar' — nothing
-- outside this module thinks in screen space.
module Wyrdshaper.Engine
  ( -- * Window and renderer
    Gfx (..),
    withEngine,

    -- * Input
    Input,
    pollInput,
    keyHeld,
    keyTapped,
    inputQuit,
    module SDL.Input.Keyboard.Codes,

    -- * Drawing
    Color (..),
    clearFrame,
    fillWorldRect,
    drawHudBar,
    presentFrame,

    -- * Clock
    now,
  )
where

import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import Linear (V2 (..), V4 (..))
import Linear.Affine (Point (P))
import SDL (($=))
import qualified SDL
import SDL.Input.Keyboard.Codes

-- | The window and renderer handles plus the (fixed) window size in pixels.
data Gfx = Gfx
  { gfxRenderer :: SDL.Renderer,
    gfxWindow :: SDL.Window,
    gfxWinSize :: V2 Int
  }

-- | Initialize SDL, open a vsynced window, run the action, tear down.
withEngine :: String -> V2 Int -> (Gfx -> IO a) -> IO a
withEngine title size act =
  bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit $
    bracket
      ( SDL.createWindow
          (T.pack title)
          SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> size}
      )
      SDL.destroyWindow
      $ \win ->
        bracket
          ( SDL.createRenderer
              win
              (-1)
              SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedVSyncRenderer}
          )
          SDL.destroyRenderer
          $ \r -> do
            SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend
            act (Gfx r win size)

-- | One frame's input: keys held now, keys newly pressed since the last
-- poll (key-repeat filtered out), and whether a quit was requested (window
-- close). Built once per frame and shared by every tick of that frame.
data Input = Input
  { inHeld :: Scancode -> Bool,
    inTapped :: Set.Set Scancode,
    inQuit :: Bool
  }

-- | Pump SDL events and snapshot the input state. Events must be pumped
-- before reading the keyboard state, or the held-key snapshot goes stale.
pollInput :: (MonadIO m) => m Input
pollInput = do
  payloads <- map SDL.eventPayload <$> SDL.pollEvents
  held <- SDL.getKeyboardState
  let tapped =
        Set.fromList
          [ SDL.keysymScancode (SDL.keyboardEventKeysym ked)
            | SDL.KeyboardEvent ked <- payloads,
              SDL.keyboardEventKeyMotion ked == SDL.Pressed,
              not (SDL.keyboardEventRepeat ked)
          ]
  pure
    Input
      { inHeld = held,
        inTapped = tapped,
        inQuit = SDL.QuitEvent `elem` payloads
      }

-- | Is the key held this frame?
keyHeld :: Scancode -> Input -> Bool
keyHeld sc input = inHeld input sc

-- | Was the key newly pressed this frame?
keyTapped :: Scancode -> Input -> Bool
keyTapped sc = Set.member sc . inTapped

-- | Was a window close requested this frame?
inputQuit :: Input -> Bool
inputQuit = inQuit

-- | An RGBA color with components in [0, 1].
data Color = Color !Float !Float !Float !Float

toV4 :: Color -> V4 Word8
toV4 (Color r g b a) = V4 (w8 r) (w8 g) (w8 b) (w8 a)
  where
    w8 = round . (* 255) . max 0 . min 1

-- | Clear the frame to a solid color.
clearFrame :: (MonadIO m) => Gfx -> Color -> m ()
clearFrame gfx c = do
  SDL.rendererDrawColor (gfxRenderer gfx) $= toV4 c
  SDL.clear (gfxRenderer gfx)

-- | Fill an axis-aligned rect given in world coordinates: the camera is the
-- world point at the window center, @center@ is the rect's AABB center, and
-- @size@ its full extents.
fillWorldRect :: (MonadIO m) => Gfx -> V2 Int -> V2 Int -> V2 Int -> Color -> m ()
fillWorldRect gfx (V2 camX camY) (V2 cx cy) (V2 w h) c =
  fillScreenRect gfx (V2 sx sy) (V2 w h) c
  where
    V2 winW winH = gfxWinSize gfx
    sx = cx - w `div` 2 - camX + winW `div` 2
    sy = camY + winH `div` 2 - (cy + h `div` 2) -- the y-flip

-- | One filled fraction-bar (dark backing, colored fill), @slot@ bars up
-- from the bottom-left corner of the screen.
drawHudBar :: (MonadIO m) => Gfx -> Int -> Float -> Color -> m ()
drawHudBar gfx slot frac c = do
  let V2 _ winH = gfxWinSize gfx
      barW = 160
      barH = 10
      top = winH - (12 + 16 * slot) - barH
      fill = round (fromIntegral barW * max 0 (min 1 frac))
  fillScreenRect gfx (V2 12 top) (V2 barW barH) (Color 0.1 0.1 0.12 0.9)
  fillScreenRect gfx (V2 12 top) (V2 fill barH) c

-- | Fill a rect given as its top-left corner and size in screen pixels.
fillScreenRect :: (MonadIO m) => Gfx -> V2 Int -> V2 Int -> Color -> m ()
fillScreenRect gfx pos size c = do
  SDL.rendererDrawColor (gfxRenderer gfx) $= toV4 c
  SDL.fillRect
    (gfxRenderer gfx)
    (Just (SDL.Rectangle (P (fromIntegral <$> pos)) (fromIntegral <$> size)))

-- | Present the finished frame (blocks on vsync).
presentFrame :: (MonadIO m) => Gfx -> m ()
presentFrame = SDL.present . gfxRenderer

-- | Seconds since SDL initialization.
now :: (MonadIO m) => m Double
now = SDL.time
