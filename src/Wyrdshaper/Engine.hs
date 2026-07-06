-- | The SDL2-facing layer: window\/renderer lifecycle, per-frame input
-- snapshots, the camera transform, immediate-mode rect drawing, and text.
--
-- The only module that imports SDL (sdl2-ttf's @SDL.Font@ included). It owns
-- the sole flip from world coordinates (pixels, origin bottom-left, y up) to
-- SDL screen coordinates (origin top-left, y down): 'fillWorldRect' — world
-- rendering never thinks in screen space. UI overlays ('drawHudBar',
-- 'fillUiRect', 'drawText') are the exception: they take screen-space
-- positions (top-left origin, y down) directly.
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
    mousePos,
    mouseHeld,
    mousePressed,
    mouseReleased,
    mouseWheel,
    demoInput,
    module SDL.Input.Keyboard.Codes,

    -- * Drawing
    Color (..),
    clearFrame,
    fillWorldRect,
    drawHudBar,
    fillUiRect,
    presentFrame,

    -- * Text
    FontKind (..),
    drawText,
    measureText,

    -- * Clock
    now,
  )
where

import Control.Exception (SomeException, bracket, bracket_, catch)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import Linear (V2 (..), V4 (..))
import Linear.Affine (Point (P))
import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import SDL.Input.Keyboard.Codes

-- | The window and renderer handles, the (fixed) window size in pixels, and
-- the loaded UI fonts.
data Gfx = Gfx
  { gfxRenderer :: SDL.Renderer,
    gfxWindow :: SDL.Window,
    gfxWinSize :: V2 Int,
    gfxTextFont :: Font.Font,
    gfxTitleFont :: Font.Font
  }

-- | UI fonts from Franuka's Fantasy RPG UI pack. @assets/@ is gitignored
-- (the pack may not be redistributed), so a fresh clone must supply it; see
-- the credits in README. These are pixel fonts: the text face stays crisp at
-- multiples of 8 pt, the title face at multiples of 11 pt.
uiTextFontPath, uiTitleFontPath :: FilePath
uiTextFontPath = "assets/ui_pack/Fonts/FantasyRPGtext (size 8).ttf"
uiTitleFontPath = "assets/ui_pack/Fonts/FantasyRPGtitle (size 11).ttf"

uiTextFontSize, uiTitleFontSize :: Int
uiTextFontSize = 16
uiTitleFontSize = 22

loadUiFont :: FilePath -> Int -> IO Font.Font
loadUiFont path pts =
  Font.load path pts `catch` \(e :: SomeException) ->
    ioError . userError $
      "cannot load UI font "
        ++ show path
        ++ " (assets/ is gitignored and must be supplied separately): "
        ++ show e

-- | Initialize SDL, open a vsynced window, load the UI fonts, run the
-- action, tear down.
withEngine :: String -> V2 Int -> (Gfx -> IO a) -> IO a
withEngine title size act =
  bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit $
    bracket_ Font.initialize Font.quit $
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
              bracket (loadUiFont uiTextFontPath uiTextFontSize) Font.free $ \textFont ->
                bracket (loadUiFont uiTitleFontPath uiTitleFontSize) Font.free $ \titleFont ->
                  act (Gfx r win size textFont titleFont)

-- | One frame's input: keys held now, keys newly pressed since the last
-- poll (key-repeat filtered out), whether a quit was requested (window
-- close), and the mouse snapshot (position in window\/UI screen space,
-- left-button state and edges, wheel travel). Built once per frame and
-- shared by every tick of that frame.
data Input = Input
  { inHeld :: Scancode -> Bool,
    inTapped :: Set.Set Scancode,
    inQuit :: Bool,
    inMousePos :: V2 Int,
    inMouseHeld :: Bool,
    inMousePressed :: Bool,
    inMouseReleased :: Bool,
    inWheel :: Int
  }

-- | Pump SDL events and snapshot the input state. Events must be pumped
-- before reading the keyboard state, or the held-key snapshot goes stale.
pollInput :: (MonadIO m) => m Input
pollInput = do
  payloads <- map SDL.eventPayload <$> SDL.pollEvents
  held <- SDL.getKeyboardState
  P mp <- SDL.getAbsoluteMouseLocation
  buttons <- SDL.getMouseButtons
  let tapped =
        Set.fromList
          [ SDL.keysymScancode (SDL.keyboardEventKeysym ked)
            | SDL.KeyboardEvent ked <- payloads,
              SDL.keyboardEventKeyMotion ked == SDL.Pressed,
              not (SDL.keyboardEventRepeat ked)
          ]
      leftEdges =
        [ SDL.mouseButtonEventMotion med
          | SDL.MouseButtonEvent med <- payloads,
            SDL.mouseButtonEventButton med == SDL.ButtonLeft
        ]
      wheel =
        sum
          [ flipSign (SDL.mouseWheelEventDirection wed) (fromIntegral wy)
            | SDL.MouseWheelEvent wed <- payloads,
              let V2 _ wy = SDL.mouseWheelEventPos wed
          ]
      flipSign d = case d of SDL.ScrollFlipped -> negate; _ -> id
  pure
    Input
      { inHeld = held,
        inTapped = tapped,
        inQuit = SDL.QuitEvent `elem` payloads,
        inMousePos = fromIntegral <$> mp,
        inMouseHeld = buttons SDL.ButtonLeft,
        inMousePressed = SDL.Pressed `elem` leftEdges,
        inMouseReleased = SDL.Released `elem` leftEdges,
        inWheel = wheel
      }

-- | A synthetic input snapshot (no keyboard, no quit, no wheel) for the
-- headless demo driver, which has no real mouse to move: position, held,
-- pressed edge, released edge.
demoInput :: V2 Int -> Bool -> Bool -> Bool -> Input
demoInput p h pr re = Input (const False) Set.empty False p h pr re 0

-- | Is the key held this frame?
keyHeld :: Scancode -> Input -> Bool
keyHeld sc input = inHeld input sc

-- | Was the key newly pressed this frame?
keyTapped :: Scancode -> Input -> Bool
keyTapped sc = Set.member sc . inTapped

-- | Was a window close requested this frame?
inputQuit :: Input -> Bool
inputQuit = inQuit

-- | Mouse position in screen pixels (top-left origin — UI overlay space).
mousePos :: Input -> V2 Int
mousePos = inMousePos

-- | Is the left button down now?
mouseHeld :: Input -> Bool
mouseHeld = inMouseHeld

-- | Did the left button go down this frame? (Both edges can be true in
-- one frame on a fast tap.)
mousePressed :: Input -> Bool
mousePressed = inMousePressed

-- | Did the left button come up this frame?
mouseReleased :: Input -> Bool
mouseReleased = inMouseReleased

-- | Vertical wheel travel this frame (positive = away from the user).
mouseWheel :: Input -> Int
mouseWheel = inWheel

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
  fillUiRect gfx (V2 sx sy) (V2 w h) c
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
  fillUiRect gfx (V2 12 top) (V2 barW barH) (Color 0.1 0.1 0.12 0.9)
  fillUiRect gfx (V2 12 top) (V2 fill barH) c

-- | Fill a UI rect given as its top-left corner and size in screen pixels.
fillUiRect :: (MonadIO m) => Gfx -> V2 Int -> V2 Int -> Color -> m ()
fillUiRect gfx pos size c = do
  SDL.rendererDrawColor (gfxRenderer gfx) $= toV4 c
  SDL.fillRect
    (gfxRenderer gfx)
    (Just (SDL.Rectangle (P (fromIntegral <$> pos)) (fromIntegral <$> size)))

-- | Which of the two loaded UI fonts to draw with.
data FontKind = TextFont | TitleFont

uiFont :: Gfx -> FontKind -> Font.Font
uiFont gfx fk = case fk of
  TextFont -> gfxTextFont gfx
  TitleFont -> gfxTitleFont gfx

-- | Draw a string with its top-left corner at a screen-pixel position.
-- Rendered fresh each call (fine for the paused editor screen; cache
-- textures here if a live HUD ever needs lots of text).
drawText :: (MonadIO m) => Gfx -> FontKind -> V2 Int -> Color -> String -> m ()
drawText gfx fk pos c s
  | null s = pure ()
  | otherwise = do
      surf <- Font.blended (uiFont gfx fk) (toV4 c) (T.pack s)
      tex <- SDL.createTextureFromSurface (gfxRenderer gfx) surf
      SDL.freeSurface surf
      info <- SDL.queryTexture tex
      let size = V2 (SDL.textureWidth info) (SDL.textureHeight info)
      SDL.copy
        (gfxRenderer gfx)
        tex
        Nothing
        (Just (SDL.Rectangle (P (fromIntegral <$> pos)) size))
      SDL.destroyTexture tex

-- | Rendered size of a string in screen pixels.
measureText :: (MonadIO m) => Gfx -> FontKind -> String -> m (V2 Int)
measureText gfx fk s = do
  (w, h) <- Font.size (uiFont gfx fk) (T.pack s)
  pure (V2 w h)

-- | Present the finished frame (blocks on vsync).
presentFrame :: (MonadIO m) => Gfx -> m ()
presentFrame = SDL.present . gfxRenderer

-- | Seconds since SDL initialization.
now :: (MonadIO m) => m Double
now = SDL.time
