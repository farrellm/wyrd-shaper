{-# LANGUAGE TypeApplications #-}

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
    renderWithCamera,

    -- * Windowing and input
    Window (..),
    Keys,
    Key (..),
    keyPressed,
    keyJustPressed,
    keyJustUnpressed,
    runAccessGLFW,
    enableVSync,

    -- * Helpers
    Color (..),
    spawnColoredRect,
    sharedRectMesh,
    colorMaterial,
    registerInstances,
  )
where

import Aztecs
import Aztecs.GL.D2
import Aztecs.GL.Internal
  ( MaterialState (..),
    MeshState (..),
    RenderGroupKey (..),
    RenderGroups (..),
    renderGroups,
  )
import Aztecs.GLFW
  ( Key (..),
    Keys,
    RawWindow (..),
    Window (..),
    keyJustPressed,
    keyJustUnpressed,
    keyPressed,
    runAccessGLFW,
  )
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
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

-- | Compile a rectangle once and return its mesh entity, for sharing across
-- many instances via 'registerInstances'. Implemented by spawning a hidden
-- prototype: @Rectangle@'s insert hook owns mesh compilation (and needs the
-- parent window's GL context), so we let it run and take the mesh it made.
sharedRectMesh :: (MonadIO m) => EntityID -> Rectangle -> Access m EntityID
sharedRectMesh winE rect = do
  protoE <- spawn $ bundle (Parent winE)
  insert protoE $ bundle rect
  mOf <- lookup protoE
  case mOf of
    Just (OfMesh meshE) -> pure meshE
    Nothing -> error "sharedRectMesh: Rectangle insert hook produced no mesh (is the parent a window?)"

-- | Spawn a solid-color material entity for sharing across instances.
colorMaterial :: (MonadIO m) => Color -> Access m EntityID
colorMaterial (Color r g b a) = spawn $ bundle (color r g b a)

-- | Register many entities to render with a shared mesh and material, in one
-- update. Equivalent to a @registerRenderable@ per entity, but that
-- re-inserts the whole 'RenderGroups' map (and prints a debug trace) each
-- call — quadratic over a tilemap's worth of entities.
--
-- Instances need only a 'Transform2D'; no @Parent@, @OfMesh@ or @OfMaterial@
-- components are required for rendering.
registerInstances :: (MonadIO m) => EntityID -> EntityID -> [EntityID] -> Access m ()
registerInstances meshE matE es = do
  (rgE, RenderGroups groups) <- renderGroups
  let key = RenderGroupKey meshE matE
      groups' = Map.insertWith Set.union key (Set.fromList es) groups
  insert rgE $ bundle (RenderGroups groups')

-- | Enable vsync on a window entity's GL context (call once after spawning).
enableVSync :: (MonadIO m) => EntityID -> Access m ()
enableVSync winE = do
  mRaw <- lookup winE
  forM_ mRaw $ \(RawWindow raw _) -> liftIO $ do
    GLFW.makeContextCurrent (Just raw)
    GLFW.swapInterval 1

-- | 'Aztecs.GL.D2.render' with a camera: the world is drawn translated so
-- the given world-pixel position sits at the window center. A copy of the
-- upstream renderer (which hardcodes a fixed origin) with one modelview
-- translate added; the per-mesh draw uses @preservingMatrix@, so the
-- translate survives across render groups.
renderWithCamera :: (MonadIO m) => V2 Int -> Access m ()
renderWithCamera (V2 camX camY) = do
  windows <- system . readQuery $ (,) <$> query @_ @Window <*> query @_ @RawWindow
  forM_ windows $ \(window, RawWindow raw _) -> do
    liftIO $ do
      GLFW.makeContextCurrent (Just raw)
      GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral $ windowWidth window) (fromIntegral $ windowHeight window))
      GL.clearColor $= GL.Color4 0.03 0.03 0.05 1
      GL.clear [GL.ColorBuffer]
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho 0 (fromIntegral $ windowWidth window) 0 (fromIntegral $ windowHeight window) (-1) 1
      GL.matrixMode $= GL.Modelview 0
      GL.loadIdentity
      GL.translate $
        GL.Vector3
          (fromIntegral $ windowWidth window `div` 2 - camX)
          (fromIntegral $ windowHeight window `div` 2 - camY)
          (0 :: GL.GLfloat)
    (_, RenderGroups groups) <- renderGroups
    forM_ (Map.toList groups) $ \(RenderGroupKey meshE matE, es) -> drawGroup meshE matE es

-- | Draw one render group (copy of the unexported upstream @renderGroup@).
drawGroup :: (MonadIO m) => EntityID -> EntityID -> Set.Set EntityID -> Access m ()
drawGroup meshE matE entitySet = do
  mMeshState <- do
    mMeshState <- lookup meshE
    case mMeshState of
      Just ms -> return $ Just ms
      Nothing -> do
        mMesh <- lookup meshE
        case mMesh of
          Just mesh -> do
            ms <- liftIO $ unMesh mesh
            insert meshE $ bundle ms
            return $ Just ms
          Nothing -> return Nothing
  mMatState <- do
    mMatState <- lookup matE
    case mMatState of
      Just ms -> return $ Just ms
      Nothing -> do
        mMat <- lookup matE
        case mMat of
          Just mat -> do
            ms <- liftIO $ unMaterial mat
            insert matE $ bundle ms
            return $ Just ms
          Nothing -> return Nothing
  case (mMeshState, mMatState) of
    (Just meshState, Just matState) -> do
      transforms <- forM (Set.toList entitySet) lookup
      liftIO $ do
        materialPush matState
        forM_ transforms $ \mTrans ->
          case mTrans of
            Just (GlobalTransform trans) -> drawMeshAt meshState trans
            Nothing -> return ()
        materialPop matState
    _ -> return ()

-- | Copy of the unexported upstream @renderMeshWithTransform@.
drawMeshAt :: MeshState -> Transform2D -> IO ()
drawMeshAt md t = do
  let V2 tx ty = transformTranslation t
      V2 sx sy = transformScale t
      rot = transformRotation t
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 (realToFrac tx) (realToFrac ty) (0 :: GL.GLfloat)
    GL.rotate (realToFrac rot) $ GL.Vector3 0 0 (1 :: GL.GLfloat)
    GL.scale (realToFrac sx) (realToFrac sy) (1 :: GL.GLfloat)
    meshPush md
    meshPop md
