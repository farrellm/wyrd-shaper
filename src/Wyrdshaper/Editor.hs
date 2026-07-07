-- | The in-game glyph editor (M3): UI state, per-frame input handling, and
-- panel drawing.
--
-- All panel geometry is built once per frame by 'buildLayout' and consumed
-- by both 'updateEditor' (mouse hit-testing) and 'drawEditor' (pixels), so
-- the two can never disagree about where a thing is. 'updateEditor' is pure
-- given the 'Layout' and the frame's 'Input'; only 'buildLayout' (font
-- measuring) and 'drawEditor' touch the renderer. The panel draws in screen
-- space (top-left origin) via 'drawText'\/'fillUiRect' — the UI-overlay
-- exception to the world-coordinate convention.
module Wyrdshaper.Editor
  ( EditorState (..),
    EdView (..),
    EditorOut (..),
    openEditor,

    -- * Layout (fields exposed so the demo driver can aim synthetic clicks)
    Rect (..),
    PieceBox (..),
    RowBox (..),
    GapBox (..),
    BodyBox (..),
    MenuBox (..),
    Layout (..),
    BlockGeom (..),
    blockGeom,
    buildLayout,
    updateEditor,
    drawEditor,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.List (find, findIndex, nub, unsnoc)
import Data.Maybe (fromMaybe)
import Linear (V2 (..))
import Wyrdshaper.Engine
import Wyrdshaper.Glyph
import Wyrdshaper.Spell (Stmt, spellSize, willpowerMax)
import Wyrdshaper.Spellbook

-- * State

-- | What the mouse button is doing to the editor. A press is not yet a
-- drag: it becomes one when the pointer travels past 'dragThreshold'
-- (release before that is a click).
data DragState
  = DragIdle
  | -- | Button went down at this position on this target.
    DragPressed (V2 Int) PressTarget
  | Dragging DragSource

data PressTarget
  = PressPalette Int
  | PressRow
      { prIndex :: Int,
        prPath :: Path,
        prHole :: Bool,
        -- | The field piece under the press, if any.
        prField :: Maybe Int
      }

data DragSource = DragPalette Int | DragRow Path

-- | An open dropdown menu over one field of one row.
data MenuState = MenuState
  { menuPath :: Path,
    menuField :: Int,
    menuOptions :: [(String, ENode)]
  }

-- | Which presentation the editor is showing. Both views edit the same
-- buffer through the same 'updateEditor'; only geometry and pixels differ.
data EdView = ViewBlocks | ViewRows
  deriving (Eq)

data EditorState = EditorState
  { -- | Slot being edited (0-based).
    edSlot :: Int,
    -- | Current presentation; 'V' toggles it, preserving everything else.
    edView :: EdView,
    edBuf :: Spell,
    -- | Index into @'flatten' edBuf@.
    edCursor :: Int,
    -- | Selected field within the cursor row.
    edField :: Int,
    -- | One-line message under the panel (errors, save notes).
    edStatus :: String,
    -- | Last seen mouse position; drawing reads this, never raw input,
    -- so the demo's synthetic frames render truthful pixels.
    edMouse :: V2 Int,
    edDrag :: DragState,
    edMenu :: Maybe MenuState
  }

-- | Open a slot for editing. A spell outside the glyph subset opens as an
-- empty buffer (it stays untouched in the book unless committed over).
openEditor :: Int -> Spellbook -> EditorState
openEditor slot book = case decompile (slotSpell slot book) of
  Just buf -> fresh buf ""
  Nothing -> fresh [] "SLOT TOO INTRICATE FOR GLYPHS - STARTING EMPTY"
  where
    fresh buf status =
      EditorState
        { edSlot = slot,
          edView = ViewBlocks,
          edBuf = buf,
          edCursor = 0,
          edField = 0,
          edStatus = status,
          edMouse = V2 0 0,
          edDrag = DragIdle,
          edMenu = Nothing
        }

-- * Layout

-- | A screen-space rectangle: top-left corner and size.
data Rect = Rect {rPos :: V2 Int, rSize :: V2 Int}

inRect :: V2 Int -> Rect -> Bool
inRect (V2 px py) (Rect (V2 x y) (V2 w h)) =
  px >= x && px < x + w && py >= y && py < y + h

-- | One drawn piece of a row; 'pbField' 'Just' pieces are clickable.
data PieceBox = PieceBox
  { pbField :: Maybe Int,
    pbText :: String,
    pbRect :: Rect
  }

data RowBox = RowBox
  { -- | Flat index — the 'edCursor' domain.
    rbIndex :: Int,
    rbPath :: Path,
    rbKind :: RowKind,
    -- | The full-width band (also the cursor highlight).
    rbRect :: Rect,
    rbPieces :: [PieceBox]
  }

-- | A snap target: an insertion point with its indicator-line geometry.
data GapBox = GapBox
  { gbPoint :: InsPoint,
    gbLineX :: Int,
    gbY :: Int
  }

-- | Draw-only geometry of one container block's C-shape in the block view:
-- the arm running down the left of the inset children and the bar closing
-- the C underneath them. Classic view builds none.
data BodyBox = BodyBox
  { bodyPath :: Path,
    bodyArm :: Rect,
    bodyBar :: Rect
  }

data MenuBox = MenuBox
  { mbRect :: Rect,
    -- | option index, label, item rect
    mbItems :: [(Int, String, Rect)]
  }

data Layout = Layout
  { layRows :: [RowBox],
    layGaps :: [GapBox],
    layBodies :: [BodyBox],
    layPalette :: [(Int, Rect)],
    -- | The whole palette column: the delete drop zone.
    layPaletteZone :: Rect,
    layMenu :: Maybe MenuBox
  }

rowH :: Int
rowH = 18

rowY :: Int -> Int
rowY i = 48 + i * rowH

rowX :: Int -> Int
rowX depth = 210 + 20 * depth

holeText :: String
holeText = "( EMPTY )"

-- | Measure every rect the editor draws or hit-tests this frame. Depends
-- only on 'Gfx' (fonts, window size) and the editor state — never on live
-- input — so update and draw see identical geometry. Both views produce
-- the same 'Layout' shape, so 'updateEditor' never knows which is showing.
buildLayout :: (MonadIO m) => Gfx -> EditorState -> m Layout
buildLayout gfx st = case edView st of
  ViewRows -> buildRowLayout gfx st
  ViewBlocks -> buildBlockLayout gfx st

-- | The classic view: one full-width text row per glyph, indented by depth.
buildRowLayout :: (MonadIO m) => Gfx -> EditorState -> m Layout
buildRowLayout gfx st = do
  let V2 winW winH = gfxWinSize gfx
      rows = flatten (edBuf st)

  rowBoxes <- forM (zip [0 ..] rows) $ \(i, r) -> do
    let y = rowY i
        band = Rect (V2 200 (y - 2)) (V2 (winW - 216) rowH)
        pieceBoxes _ [] = pure []
        pieceBoxes x ((mf, s) : ps) = do
          V2 w _ <- measureText gfx TextFont s
          rest <- pieceBoxes (x + w + 8) ps
          pure (PieceBox mf s (Rect (V2 (x - 2) (y - 2)) (V2 (w + 4) rowH)) : rest)
    pieces <- case rowKind r of
      RHole -> pure []
      RNode n _ -> pieceBoxes (rowX (rowDepth r)) (rowPieces n)
    pure (RowBox i (rowPath r) (rowKind r) band pieces)

  paletteRects <- paletteLayout gfx
  menu <- menuLayout gfx st rowBoxes

  pure
    Layout
      { layRows = rowBoxes,
        layGaps =
          [ GapBox ip (rowX (ipDepth ip)) (rowY (ipBeforeRow ip) - 2)
            | ip <- insertionPoints (edBuf st)
          ],
        layBodies = [],
        layPalette = paletteRects,
        layPaletteZone = paletteZone winH,
        layMenu = menu
      }

-- | The palette column rects, shared by both views (same hit-testing and
-- delete-zone semantics either way).
paletteLayout :: (MonadIO m) => Gfx -> m [(Int, Rect)]
paletteLayout gfx = forM (zip [0 ..] paletteEntries) $ \(i, (label, _)) -> do
  V2 w _ <- measureText gfx TextFont (paletteLabel i label)
  pure (i, Rect (V2 12 (rowY i - 2)) (V2 (w + 8) rowH))

paletteZone :: Int -> Rect
paletteZone winH = Rect (V2 0 40) (V2 196 (winH - 100))

-- | The open dropdown's geometry, anchored under its field piece. Purely
-- rect-driven, so it works over both views' piece boxes.
menuLayout :: (MonadIO m) => Gfx -> EditorState -> [RowBox] -> m (Maybe MenuBox)
menuLayout gfx st rowBoxes = case edMenu st of
  Nothing -> pure Nothing
  Just ms -> do
    let V2 _ winH = gfxWinSize gfx
        anchor = do
          rb <- find ((== menuPath ms) . rbPath) rowBoxes
          find ((== Just (menuField ms)) . pbField) (rbPieces rb)
    case anchor of
      Nothing -> pure Nothing -- stale menu; menuStep will just close it
      Just pb -> do
        let labels = map fst (menuOptions ms)
        widths <- mapM (fmap (\(V2 w _) -> w) . measureText gfx TextFont) labels
        let menuW = maximum (40 : widths) + 12
            menuH = length labels * rowH + 8
            Rect (V2 ax ay) (V2 _ ah) = pbRect pb
            top = max 8 (min (winH - 8 - menuH) (ay + ah + 2))
            item (j, s) =
              (j, s, Rect (V2 ax (top + 4 + j * rowH)) (V2 (menuW - 8) rowH))
        pure . Just $
          MenuBox
            { mbRect = Rect (V2 (ax - 4) top) (V2 menuW menuH),
              mbItems = map item (zip [0 ..] labels)
            }

paletteLabel :: Int -> String -> String
paletteLabel i label = show (i + 1) ++ " " ++ label

-- * Block view geometry

-- Blocks stack from the same origin as the classic rows. No scrolling is
-- needed: the worst 'willpowerMax'-glyph document (all containers, one
-- socket) is ~305 px tall from blkY0 — well inside the window.
blkX0, blkY0 :: Int
blkX0 = 210
blkY0 = 48

blkHeaderH, blkArmW, blkArmVis, blkGapH, blkBarH, blkSocketH, blkPadX, chipH, chipPad :: Int
blkHeaderH = 22 -- header bar height
blkArmW = 16 -- child indent = the C-arm span
blkArmVis = 10 -- drawn arm width (visual gap to the children)
blkGapH = 5 -- sibling spacing band = the snap slot
blkBarH = 8 -- bottom bar closing the C
blkSocketH = 20 -- empty-body socket height
blkPadX = 6 -- header inner horizontal padding
chipH = 16 -- field-chip height
chipPad = 3 -- field-chip padding around its text

-- | The block view's geometry: the same box types 'updateEditor' consumes,
-- plus the draw-only C-shapes.
data BlockGeom = BlockGeom
  { bgRows :: [RowBox],
    bgGaps :: [GapBox],
    bgBodies :: [BodyBox]
  }

-- | Pure block layout: text widths in, geometry out (repl-testable with a
-- fake width like @(*8) . length@). The recursion mirrors 'flatten' /
-- 'insertionPoints' exactly — same flat indices, paths, scopes, and point
-- order — so 'edCursor' and the drag logic mean the same thing in both
-- views.
blockGeom :: (String -> Int) -> Spell -> BlockGeom
blockGeom widthOf sp =
  let (_, _, rows, gaps, bodies) = goList [] 0 [] 0 blkY0 sp
   in BlockGeom rows gaps bodies
  where
    -- goList path depth scope i y ns -> (i', y', rows, gaps, bodies)
    goList path depth _ i y [] =
      let x = blkX0 + depth * blkArmW
          socketW = widthOf holeText + 2 * blkPadX
       in ( i + 1,
            y + blkSocketH,
            [RowBox i (path ++ [0]) RHole (Rect (V2 x y) (V2 socketW blkSocketH)) []],
            [GapBox (InsPoint (path ++ [0]) i depth True) x (y + blkSocketH `div` 2)],
            []
          )
    goList path depth scope i0 y0 ns = go i0 y0 0 ns
      where
        x = blkX0 + depth * blkArmW
        endGap i y j = GapBox (InsPoint (path ++ [j]) i depth False) x (y + blkGapH `div` 2)
        go i y j [] = (i, y + blkGapH, [], [endGap i y j], [])
        go i y j (n : rest) =
          let gapHere = endGap i y j
              yTop = y + blkGapH
              p = path ++ [j]
              (pieces, headerW) = headerPieces x yTop n
              headerRow = RowBox i p (RNode n scope) (Rect (V2 x yTop) (V2 headerW blkHeaderH)) pieces
              yBody = yTop + blkHeaderH
              container scope' b =
                let (i', yEnd, rs, gs, bs) = goList p (depth + 1) scope' (i + 1) yBody b
                    arm = Rect (V2 x yBody) (V2 blkArmVis (yEnd - yBody))
                    bar = Rect (V2 x yEnd) (V2 (max headerW (blkArmW + 32)) blkBarH)
                 in (i', yEnd + blkBarH, rs, gs, bs ++ [BodyBox p arm bar])
              (i1, y1, subRows, subGaps, subBodies) = case n of
                EInvoke _ _ -> (i + 1, yBody, [], [], [])
                ERepeat _ b -> container scope b
                EIf _ _ b -> container scope b
                ELet nm _ b -> container (scope ++ [nm]) b
              (i2, y2, moreRows, moreGaps, moreBodies) = go i1 y1 (j + 1) rest
           in ( i2,
                y2,
                headerRow : subRows ++ moreRows,
                gapHere : subGaps ++ moreGaps,
                subBodies ++ moreBodies
              )

    -- Lay a header's pieces left to right; field pieces get chip rects
    -- (padded, the hit target for the dropdown), connectives tight ones.
    headerPieces x0 yTop n =
      let chipY = yTop + (blkHeaderH - chipH) `div` 2
          go _ [] = ([], 0)
          go cx ((mf, s) : ps) =
            let w = case mf of
                  Just _ -> widthOf s + 2 * chipPad
                  Nothing -> widthOf s
                (rest, end) = go (cx + w + 6) ps
             in (PieceBox mf s (Rect (V2 cx chipY) (V2 w chipH)) : rest, max (cx + w) end)
          (pieces, endX) = go (x0 + blkPadX) (rowPieces n)
       in (pieces, endX - x0 + blkPadX)

-- | The block view: colored stacked blocks; containers wrap their inset
-- children in a C-shape. Same 'Layout' contract as the classic view.
buildBlockLayout :: (MonadIO m) => Gfx -> EditorState -> m Layout
buildBlockLayout gfx st = do
  let V2 _ winH = gfxWinSize gfx
      strings =
        holeText
          : [s | Row _ _ (RNode n _) <- flatten (edBuf st), (_, s) <- rowPieces n]
  widths <- forM (nub strings) $ \s -> do
    V2 w _ <- measureText gfx TextFont s
    pure (s, w)
  let widthOf s = fromMaybe 0 (lookup s widths)
      BlockGeom rowBoxes gaps bodies = blockGeom widthOf (edBuf st)

  paletteRects <- paletteLayout gfx
  menu <- menuLayout gfx st rowBoxes

  pure
    Layout
      { layRows = rowBoxes,
        layGaps = gaps,
        layBodies = bodies,
        layPalette = paletteRects,
        layPaletteZone = paletteZone winH,
        layMenu = menu
      }

-- * Update

data EditorOut
  = EdContinue EditorState
  | -- | Compiled and within Willpower: write to this slot and close.
    EdCommit Int Stmt
  | EdCancel

paletteKeys :: [Scancode]
paletteKeys = [Scancode1, Scancode2, Scancode3, Scancode4, Scancode5, Scancode6]

-- | Pointer travel (squared px) that turns a press into a drag.
dragThreshold :: Int
dragThreshold = 16

-- | One frame of editor input. Handler precedence is the Esc story: an
-- open menu consumes everything (Esc closes it), then a live press\/drag
-- (Esc cancels it), and only in idle does the keyboard chain run (Esc
-- cancels the editor).
updateEditor :: Layout -> Input -> Spellbook -> EditorState -> EditorOut
updateEditor lay input book st0
  | Just ms <- edMenu st = menuStep ms
  | DragPressed origin tgt <- edDrag st = pressedStep origin tgt
  | Dragging src <- edDrag st = dragStep src
  | mousePressed input = pressStep
  | otherwise = keyboardStep lay input book st
  where
    st = st0 {edMouse = mousePos input}
    mp = mousePos input

    rows = flatten (edBuf st)
    continue s = EdContinue s

    -- An open dropdown: apply, keep, or dismiss.
    menuStep ms
      | keyTapped ScancodeEscape input = continue st {edMenu = Nothing}
      | mousePressed input = case layMenu lay of
          Nothing -> continue st {edMenu = Nothing}
          Just mb -> case find (\(_, _, r) -> mp `inRect` r) (mbItems mb) of
            Just (j, _, _)
              | (_, node) : _ <- drop j (menuOptions ms) ->
                  continue $ case modifyAt (menuPath ms) (const node) (edBuf st) of
                    Just buf' ->
                      st
                        { edBuf = buf',
                          edField = menuField ms,
                          edMenu = Nothing,
                          edStatus = ""
                        }
                    Nothing -> st {edMenu = Nothing, edStatus = "THE GLYPHS RESIST"}
            _
              | mp `inRect` mbRect mb -> continue st -- inside the panel, between items
              | otherwise -> continue st {edMenu = Nothing} -- click-away: swallow
      | otherwise = continue st

    -- Button down, not yet a drag.
    pressedStep origin tgt
      | mouseReleased input = continue (clickAction tgt st {edDrag = DragIdle})
      | keyTapped ScancodeEscape input = continue st {edDrag = DragIdle}
      | mouseHeld input,
        farEnough origin =
          continue $ case tgt of
            PressPalette i -> st {edDrag = Dragging (DragPalette i)}
            PressRow {prHole = True} -> st {edDrag = DragIdle} -- holes aren't draggable
            PressRow {prPath = p} -> st {edDrag = Dragging (DragRow p)}
      | mouseHeld input = continue st
      | otherwise = continue st {edDrag = DragIdle} -- release lost off-window

    farEnough (V2 ox oy) =
      let V2 px py = mp
       in (px - ox) * (px - ox) + (py - oy) * (py - oy) > dragThreshold

    -- A live drag: drop, cancel, or keep floating.
    dragStep src
      | mouseReleased input = continue (dropAction src st {edDrag = DragIdle})
      | keyTapped ScancodeEscape input = continue st {edDrag = DragIdle}
      | mouseHeld input = continue st
      | otherwise = continue st {edDrag = DragIdle} -- release lost off-window

    -- Fresh button-down: resolve what was hit. Hitting a row immediately
    -- syncs the keyboard cursor; press-and-release within one frame is a
    -- fast click.
    pressStep = case hitTest lay mp of
      Just tgt
        | mouseReleased input -> continue (clickAction tgt (selectOnPress tgt st))
        | otherwise ->
            continue ((selectOnPress tgt st) {edDrag = DragPressed mp tgt})
      Nothing -> continue st

    selectOnPress tgt s = case tgt of
      PressPalette _ -> s
      PressRow {prIndex = i, prField = mf} ->
        s {edCursor = i, edField = fromMaybe 0 mf, edStatus = ""}

    -- What a click (press+release without travel) does.
    clickAction tgt s = case tgt of
      PressPalette i -> insertGlyphAtCursor False i book s
      PressRow {prField = Just f, prIndex = i}
        | Just (Row p _ (RNode n scope)) <- lookupRow i ->
            case fieldOptions scope f n of
              [] -> s
              opts -> s {edMenu = Just (MenuState p f opts)}
      PressRow {} -> s -- selection already happened on press
      where
        lookupRow i = case drop i rows of r : _ | i >= 0 -> Just r; _ -> Nothing

    -- What a drop does.
    dropAction src s = case dropTarget lay src (edMouse s) of
      DropNone -> s
      DropDelete -> case src of
        DragRow p -> case deleteAt p (edBuf s) of
          Just buf' ->
            s
              { edBuf = buf',
                edCursor = max 0 (min (length (flatten buf') - 1) (edCursor s)),
                edField = 0,
                edStatus = "UNSAID"
              }
          Nothing -> s {edStatus = "THE GLYPHS RESIST"}
        DragPalette _ -> s
      DropGap ip -> case src of
        DragPalette i
          | glyphCount (edBuf s) >= willpowerMax -> s {edStatus = "WILLPOWER FULL"}
          | otherwise -> case insertAt (ipPath ip) (snd (paletteEntries !! i)) (edBuf s) of
              Just buf' -> cursorTo (ipPath ip) buf' s
              Nothing -> s {edStatus = "THE GLYPHS RESIST"}
        DragRow src' -> case moveNode src' (ipPath ip) (edBuf s) of
          Just (landed, buf') -> cursorTo landed buf' s
          Nothing -> s {edStatus = "THE GLYPHS RESIST"}

    cursorTo path buf' s =
      s
        { edBuf = buf',
          edCursor = fromMaybe (edCursor s) (findIndex ((== path) . rowPath) (flatten buf')),
          edField = 0,
          edStatus = ""
        }

-- | What is under a screen point, by priority: palette entry, then a
-- row's field piece, then the row band.
hitTest :: Layout -> V2 Int -> Maybe PressTarget
hitTest lay mp =
  paletteHit `orElse` pieceHit `orElse` bandHit
  where
    orElse a b = maybe b Just a
    paletteHit =
      PressPalette . fst <$> find ((mp `inRect`) . snd) (layPalette lay)
    pieceHit = do
      rb <- find ((mp `inRect`) . rbRect) (layRows lay)
      pb <- find ((mp `inRect`) . pbRect) (rbPieces rb)
      f <- pbField pb
      pure (rowTarget (Just f) rb)
    bandHit = rowTarget Nothing <$> find ((mp `inRect`) . rbRect) (layRows lay)
    rowTarget mf rb =
      PressRow
        { prIndex = rbIndex rb,
          prPath = rbPath rb,
          prHole = case rbKind rb of RHole -> True; _ -> False,
          prField = mf
        }

-- | Where a drag would land right now. Shared by the drop itself and the
-- snap-indicator drawing, so the highlight is always the truth.
data DropTarget = DropDelete | DropGap InsPoint | DropNone

dropTarget :: Layout -> DragSource -> V2 Int -> DropTarget
dropTarget lay src mp@(V2 mx my)
  | mp `inRect` layPaletteZone lay = case src of
      DragRow _ -> DropDelete
      DragPalette _ -> DropNone -- putting a palette block back: cancel
  | otherwise = case valid of
      [] -> DropNone
      gs -> DropGap (gbPoint (minimumOn gapDist gs))
  where
    valid = case src of
      DragPalette _ -> layGaps lay
      DragRow p -> filter (not . insideDragged p . ipPath . gbPoint) (layGaps lay)
    insideDragged p q = length q > length p && take (length p) q == p
    -- nearest by y; ties (co-located end-of-body gaps) go to the line
    -- whose indent is nearest the pointer — Scratch's depth choice
    gapDist g = (abs (my - gbY g), abs (mx - gbLineX g))
    minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

-- | The keyboard chain, exactly as it has been since M3 (mouse handling
-- runs first and falls through to this in idle state).
keyboardStep :: Layout -> Input -> Spellbook -> EditorState -> EditorOut
keyboardStep _lay input book st
  | tapped ScancodeEscape || tapped ScancodeE = EdCancel
  | tapped ScancodeReturn = commit
  | tapped ScancodeTab = EdContinue switchSlot
  | tapped ScancodeV = EdContinue st {edView = flipView (edView st), edStatus = ""}
  | tapped ScancodeUp = moveCursor (-1)
  | tapped ScancodeDown = moveCursor 1
  | tapped ScancodeLeft = moveField (-1)
  | tapped ScancodeRight = moveField 1
  | tapped ScancodeMinus = cycleCur (-1)
  | tapped ScancodeEquals = cycleCur 1
  | tapped ScancodeX || tapped ScancodeBackspace = deleteCur
  | Just i <- findIndex tapped paletteKeys = insertGlyphAtCursor' i
  | otherwise = EdContinue st
  where
    tapped k = keyTapped k input
    shiftHeld = keyHeld ScancodeLShift input || keyHeld ScancodeRShift input

    rows = flatten (edBuf st)
    cur = max 0 (min (length rows - 1) (edCursor st))
    row = rows !! cur

    commit = case compile (edBuf st) of
      Left err -> EdContinue st {edStatus = err}
      Right stmt
        | spellSize stmt > willpowerMax ->
            EdContinue st {edStatus = "THE SPELL EXCEEDS YOUR WILLPOWER"}
        | otherwise -> EdCommit (edSlot st) stmt

    flipView ViewBlocks = ViewRows
    flipView ViewRows = ViewBlocks

    switchSlot =
      (openEditor ((edSlot st + 1) `mod` slotCount) book)
        { edStatus = "SWITCHED SLOT - UNSAVED EDITS DISCARDED",
          edView = edView st
        }

    moveCursor d =
      EdContinue
        st
          { edCursor = max 0 (min (length rows - 1) (cur + d)),
            edField = 0,
            edStatus = ""
          }

    moveField d =
      let top = case rowKind row of
            RNode n _ -> fieldCount n - 1
            RHole -> 0
       in EdContinue st {edField = max 0 (min top (edField st + d))}

    cycleCur dir = case rowKind row of
      RHole -> EdContinue st
      RNode _ scope ->
        case modifyAt (rowPath row) (cycleField scope (edField st) dir) (edBuf st) of
          Just buf' -> EdContinue st {edBuf = buf', edStatus = ""}
          Nothing -> EdContinue st {edStatus = "THE GLYPHS RESIST"}

    deleteCur = case rowKind row of
      RHole -> EdContinue st {edStatus = "NOTHING TO UNSAY HERE"}
      RNode _ _ -> case deleteAt (rowPath row) (edBuf st) of
        Just buf' ->
          EdContinue
            st
              { edBuf = buf',
                edCursor = max 0 (min (length (flatten buf') - 1) cur),
                edField = 0,
                edStatus = ""
              }
        Nothing -> EdContinue st {edStatus = "THE GLYPHS RESIST"}

    insertGlyphAtCursor' i = EdContinue (insertGlyphAtCursor shiftHeld i book st)

-- | Insert palette entry @i@ relative to the cursor row: onto a hole, or
-- as the sibling after it (before with @before@). Shared by the hotkeys
-- and palette clicks.
insertGlyphAtCursor :: Bool -> Int -> Spellbook -> EditorState -> EditorState
insertGlyphAtCursor before i _book st
  | glyphCount (edBuf st) >= willpowerMax = st {edStatus = "WILLPOWER FULL"}
  | otherwise =
      let rows = flatten (edBuf st)
          cur = max 0 (min (length rows - 1) (edCursor st))
          row = rows !! cur
          node = snd (paletteEntries !! i)
          path = case (rowKind row, unsnoc (rowPath row)) of
            (RNode _ _, Just (prefix, j))
              | not before -> prefix ++ [j + 1]
            _ -> rowPath row
       in case insertAt path node (edBuf st) of
            Just buf' ->
              st
                { edBuf = buf',
                  edCursor = fromMaybe cur (findIndex ((== path) . rowPath) (flatten buf')),
                  edField = 0,
                  edStatus = ""
                }
            Nothing -> st {edStatus = "THE GLYPHS RESIST"}

-- * Drawing

titleColor, capColor, fieldColor, selColor, plainColor, holeColor :: Color
titleColor = Color 0.95 0.85 0.55 1
capColor = Color 0.95 0.4 0.3 1
fieldColor = Color 0.92 0.92 0.95 1
selColor = Color 1 0.9 0.3 1
plainColor = Color 0.62 0.62 0.7 1
holeColor = Color 0.45 0.45 0.52 1

-- | The editor panel, drawn over the (frozen) world render, entirely from
-- the frame's 'Layout': shared chrome here, the spell area per view.
drawEditor :: (MonadIO m) => Gfx -> Layout -> EditorState -> m ()
drawEditor gfx lay st = do
  let V2 winW winH = gfxWinSize gfx
      cnt = glyphCount (edBuf st)
      atCap = cnt >= willpowerMax
      draggingRow = case edDrag st of Dragging (DragRow _) -> True; _ -> False

  fillUiRect gfx (V2 0 0) (V2 winW winH) (Color 0 0 0 0.62)

  drawText gfx TitleFont (V2 16 8) titleColor ("WYRDBOOK  SLOT " ++ show (edSlot st + 1))
  drawText
    gfx
    TitleFont
    (V2 (winW - 170) 8)
    (if atCap then capColor else titleColor)
    ("WILL " ++ show cnt ++ "/" ++ show willpowerMax)

  -- Red wash while a dragged block hovers the palette (dropping deletes).
  forM_ [() | draggingRow, edMouse st `inRect` layPaletteZone lay] $ \_ ->
    let Rect p s = layPaletteZone lay
     in fillUiRect gfx p s (Color 0.6 0.15 0.1 0.5)

  case edView st of
    ViewRows -> drawRowContent gfx lay st
    ViewBlocks -> drawBlockContent gfx lay st

  -- Dropdown menu, on top of everything but the footer.
  forM_ (layMenu lay) $ \mb -> do
    let Rect (V2 mx my) (V2 mw mh) = mbRect mb
        current = do
          ms <- edMenu st
          nodeAt (menuPath ms) (edBuf st)
        options = maybe [] menuOptions (edMenu st)
    fillUiRect gfx (V2 (mx - 2) (my - 2)) (V2 (mw + 4) (mh + 4)) (Color 0.5 0.45 0.3 0.9)
    fillUiRect gfx (V2 mx my) (V2 mw mh) (Color 0.08 0.08 0.12 0.97)
    forM_ (mbItems mb) $ \(j, s, r) -> do
      let Rect (V2 ix iy) _ = r
          isCurrent = case drop j options of
            (_, n) : _ -> Just n == current
            [] -> False
      forM_ [() | edMouse st `inRect` r] $ \_ ->
        fillUiRect gfx (rPos r) (rSize r) (Color 0.25 0.28 0.45 0.9)
      drawText gfx TextFont (V2 (ix + 2) (iy + 2)) (if isCurrent then selColor else fieldColor) s

  let viewHint = case edView st of ViewBlocks -> "V ROWS"; ViewRows -> "V BLOCKS"
  drawText
    gfx
    TextFont
    (V2 16 (winH - 44))
    plainColor
    ( "ARROWS/MOUSE EDIT  1-6 INSERT  DRAG MOVE (PALETTE: DELETE)  X DELETE  TAB SLOT  RET SAVE  "
        ++ viewHint
        ++ "  ESC"
    )
  drawText gfx TextFont (V2 16 (winH - 24)) (Color 0.95 0.6 0.3 1) (edStatus st)

-- | The classic view's spell area: palette labels, text rows, the snap
-- line, and the text drag ghost.
drawRowContent :: (MonadIO m) => Gfx -> Layout -> EditorState -> m ()
drawRowContent gfx lay st = do
  let V2 winW _ = gfxWinSize gfx
      cnt = glyphCount (edBuf st)
      atCap = cnt >= willpowerMax
      rows = layRows lay
      cur = max 0 (min (length rows - 1) (edCursor st))
      menuOpen = case edMenu st of Just _ -> True; Nothing -> False
      dragSrc = case edDrag st of Dragging src -> Just src; _ -> Nothing

  -- Palette (hotkeys 1-6); greyed once Willpower is spent.
  forM_ (zip (layPalette lay) paletteEntries) $ \((i, Rect (V2 x y) _), (label, _)) ->
    drawText
      gfx
      TextFont
      (V2 (x + 4) (y + 2))
      (if atCap then holeColor else fieldColor)
      (paletteLabel i label)

  -- The spell, one row per glyph (or hole), indented by nesting depth.
  forM_ rows $ \rb -> do
    let onCursor = rbIndex rb == cur
        Rect bandPos bandSize = rbRect rb
        hovered = not menuOpen && edMouse st `inRect` rbRect rb
    forM_ [() | hovered, not onCursor] $ \_ ->
      fillUiRect gfx bandPos bandSize (Color 0.2 0.22 0.32 0.5)
    forM_ [() | onCursor] $ \_ ->
      fillUiRect gfx bandPos bandSize (Color 0.25 0.28 0.45 0.9)
    case rbKind rb of
      RHole ->
        let V2 _ by = bandPos
         in drawText gfx TextFont (V2 (rowX (pathDepth (rbPath rb))) (by + 2)) holeColor "( EMPTY )"
      RNode _ _ -> forM_ (rbPieces rb) $ \pb -> do
        let Rect (V2 px py) _ = pbRect pb
            color = case pbField pb of
              Nothing -> plainColor
              Just k
                | onCursor && k == edField st -> selColor
                | otherwise -> fieldColor
        drawText gfx TextFont (V2 (px + 2) (py + 2)) color (pbText pb)

  -- Snap indicator + drag ghost while a drag is live.
  forM_ dragSrc $ \src -> do
    case dropTarget lay src (edMouse st) of
      DropGap ip
        | ipAtHole ip,
          Just rb <- find ((== ipPath ip) . rbPath) (layRows lay) ->
            let Rect p s = rbRect rb
             in fillUiRect gfx p s (Color 0.9 0.8 0.3 0.35)
        | otherwise ->
            let x = rowX (ipDepth ip)
                y = rowY (ipBeforeRow ip) - 2
             in fillUiRect gfx (V2 x y) (V2 (winW - 16 - x) 2) (Color 1 0.9 0.3 1)
      _ -> pure ()
    let label = case src of
          DragPalette i -> fst (paletteEntries !! i)
          DragRow p -> case nodeAt p (edBuf st) of
            Just n -> unwords [s | (_, s) <- rowPieces n]
            Nothing -> "?"
        ghostPos = edMouse st + V2 14 10
    V2 gw _ <- measureText gfx TextFont label
    fillUiRect gfx (ghostPos - V2 4 2) (V2 (gw + 8) rowH) (Color 0.12 0.14 0.22 0.85)
    drawText gfx TextFont ghostPos fieldColor label

pathDepth :: Path -> Int
pathDepth p = length p - 1

-- | Scratch-ish block color per glyph kind.
blockColor :: ENode -> Color
blockColor n = case n of
  EInvoke _ _ -> Color 0.24 0.45 0.78 1
  ERepeat _ _ -> Color 0.85 0.52 0.18 1
  EIf {} -> Color 0.33 0.62 0.33 1
  ELet {} -> Color 0.55 0.4 0.75 1

fade :: Float -> Color -> Color
fade f (Color r g b a) = Color r g b (a * f)

-- | A block-shaped fill: three non-overlapping rects shaving the corner
-- pixels to fake rounding (non-overlap keeps translucent fills uniform).
fillRounded :: (MonadIO m) => Gfx -> Rect -> Color -> m ()
fillRounded gfx (Rect (V2 x y) (V2 w h)) c = do
  fillUiRect gfx (V2 (x + 2) y) (V2 (w - 4) h) c
  fillUiRect gfx (V2 x (y + 2)) (V2 2 (h - 4)) c
  fillUiRect gfx (V2 (x + w - 2) (y + 2)) (V2 2 (h - 4)) c

-- | A 2 px rect outline from four non-overlapping strips.
strokeRect :: (MonadIO m) => Gfx -> Rect -> Color -> m ()
strokeRect gfx (Rect (V2 x y) (V2 w h)) c = do
  fillUiRect gfx (V2 x y) (V2 w 2) c
  fillUiRect gfx (V2 x (y + h - 2)) (V2 w 2) c
  fillUiRect gfx (V2 x (y + 2)) (V2 2 (h - 4)) c
  fillUiRect gfx (V2 (x + w - 2) (y + 2)) (V2 2 (h - 4)) c

-- | The block view's spell area: colored palette chips, stacked blocks with
-- C-wrapped children, the snap slot bar, and a block-shaped drag ghost.
drawBlockContent :: (MonadIO m) => Gfx -> Layout -> EditorState -> m ()
drawBlockContent gfx lay st = do
  let cnt = glyphCount (edBuf st)
      atCap = cnt >= willpowerMax
      rows = layRows lay
      cur = max 0 (min (length rows - 1) (edCursor st))
      menuOpen = case edMenu st of Just _ -> True; Nothing -> False
      dragSrc = case edDrag st of Dragging src -> Just src; _ -> Nothing

  -- Palette as colored mini-blocks (hotkeys 1-6); faded at the Willpower cap.
  forM_ (zip (layPalette lay) paletteEntries) $ \((i, r), (label, node)) -> do
    fillRounded gfx r (if atCap then fade 0.35 (blockColor node) else blockColor node)
    let Rect (V2 x y) _ = r
    drawText
      gfx
      TextFont
      (V2 (x + 4) (y + 2))
      (if atCap then holeColor else fieldColor)
      (paletteLabel i label)

  -- Container C-shapes first, so headers and children sit on top.
  forM_ (layBodies lay) $ \bb -> do
    let color = fromMaybe plainColor $ do
          rb <- find ((== bodyPath bb) . rbPath) rows
          case rbKind rb of RNode n _ -> Just (blockColor n); RHole -> Nothing
    fillUiRect gfx (rPos (bodyArm bb)) (rSize (bodyArm bb)) color
    fillRounded gfx (bodyBar bb) color

  -- The blocks: header bars (or hole sockets) with their text pieces.
  forM_ rows $ \rb -> do
    let onCursor = rbIndex rb == cur
        hovered = not menuOpen && edMouse st `inRect` rbRect rb
    case rbKind rb of
      RHole -> do
        fillUiRect gfx (rPos (rbRect rb)) (rSize (rbRect rb)) (Color 0.08 0.09 0.13 0.85)
        forM_ [() | hovered, not onCursor] $ \_ ->
          fillUiRect gfx (rPos (rbRect rb)) (rSize (rbRect rb)) (Color 1 1 1 0.1)
        let Rect (V2 x y) (V2 _ h) = rbRect rb
        drawText gfx TextFont (V2 (x + blkPadX) (y + (h - chipH) `div` 2)) holeColor holeText
      RNode n _ -> do
        fillRounded gfx (rbRect rb) (blockColor n)
        forM_ [() | hovered, not onCursor] $ \_ ->
          fillUiRect gfx (rPos (rbRect rb)) (rSize (rbRect rb)) (Color 1 1 1 0.12)
        forM_ (rbPieces rb) $ \pb -> do
          let Rect (V2 px py) (V2 pw ph) = pbRect pb
          case pbField pb of
            Just k
              | onCursor && k == edField st -> do
                  fillUiRect gfx (V2 px py) (V2 pw ph) (Color 1 0.9 0.3 0.4)
                  drawText gfx TextFont (V2 (px + chipPad) py) selColor (pbText pb)
              | otherwise -> do
                  fillUiRect gfx (V2 px py) (V2 pw ph) (Color 0 0 0 0.35)
                  drawText gfx TextFont (V2 (px + chipPad) py) fieldColor (pbText pb)
            Nothing -> drawText gfx TextFont (V2 px py) (Color 0.95 0.95 0.98 0.9) (pbText pb)
    forM_ [() | onCursor] $ \_ ->
      strokeRect gfx (rbRect rb) (Color 1 0.95 0.6 0.9)

  -- Snap indicator + drag ghost while a drag is live.
  forM_ dragSrc $ \src -> do
    case dropTarget lay src (edMouse st) of
      DropGap ip
        | ipAtHole ip,
          Just rb <- find ((== ipPath ip) . rbPath) rows ->
            let Rect p s = rbRect rb
             in fillUiRect gfx p s (Color 0.9 0.8 0.3 0.35)
        | Just gb <- find ((== ip) . gbPoint) (layGaps lay) ->
            fillUiRect gfx (V2 (gbLineX gb) (gbY gb - 2)) (V2 140 4) (Color 1 0.9 0.3 1)
        | otherwise -> pure ()
      _ -> pure ()
    let (label, color) = case src of
          DragPalette i -> let (l, n) = paletteEntries !! i in (l, blockColor n)
          DragRow p -> case nodeAt p (edBuf st) of
            Just n -> (unwords [s | (_, s) <- rowPieces n], blockColor n)
            Nothing -> ("?", plainColor)
        ghostPos = edMouse st + V2 14 10
    V2 gw _ <- measureText gfx TextFont label
    fillRounded gfx (Rect (ghostPos - V2 4 3) (V2 (gw + 8) blkHeaderH)) (fade 0.75 color)
    drawText gfx TextFont ghostPos fieldColor label
