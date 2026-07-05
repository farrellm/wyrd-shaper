-- | The thin engine-facing layer.
--
-- All aztecs packages are pre-1.0 and churn between releases; the rest of the
-- game imports this module (never aztecs directly) so migrations touch one
-- place.
module Wyrdshaper.Engine
  ( -- * ECS core
    module Aztecs,

    -- * Rendering, transforms, linear algebra
    module Aztecs.GL.D2,

    -- * Windowing and input
    Window (..),
    Keys,
    Key (..),
    keyPressed,
    keyJustPressed,
    keyJustUnpressed,
    runAccessGLFW,

    -- * Helpers
    Color (..),
    spawnColoredRect,
  )
where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GLFW
  ( Key (..),
    Keys,
    Window (..),
    keyJustPressed,
    keyJustUnpressed,
    keyPressed,
    runAccessGLFW,
  )
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (lookup)

-- | An RGBA color with components in [0, 1].
data Color = Color !Float !Float !Float !Float

-- | Spawn a solid-colored rectangle at a pixel position, parented to a window.
--
-- The @Parent@ component must be inserted before the shape: the shape's
-- insert hook looks up the parent window to find its OpenGL context.
spawnColoredRect ::
  (MonadIO m) => EntityID -> Rectangle -> Color -> V2 Int -> Access m EntityID
spawnColoredRect winE rect (Color r g b a) pos = do
  e <- spawn $ bundle (Parent winE)
  insert e $ bundle rect
  insert e $ bundle (color r g b a)
  insert e $ bundle (transform2d {transformTranslation = pos} :: Transform2D)
  pure e
