-- | The in-game glyph editor (M3): UI state, per-frame input handling, and
-- panel drawing.
--
-- 'updateEditor' is pure over the frame's 'Input', so the whole interaction
-- model is repl-testable; only 'drawEditor' touches the renderer. The panel
-- draws in screen space (top-left origin) via 'drawText'\/'fillUiRect' — the
-- UI-overlay exception to the world-coordinate convention.
module Wyrdshaper.Editor
  ( EditorState (..),
    EditorOut (..),
    openEditor,
    updateEditor,
    drawEditor,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (toUpper)
import Data.List (findIndex, unsnoc)
import Data.Maybe (fromMaybe)
import Linear (V2 (..))
import Wyrdshaper.Engine
import Wyrdshaper.Glyph
import Wyrdshaper.Spell (Op (..), Selector (..), Stmt, Verb (..), spellSize, willpowerMax)
import Wyrdshaper.Spellbook

-- * State

data EditorState = EditorState
  { -- | Slot being edited (0-based).
    edSlot :: Int,
    edBuf :: Spell,
    -- | Index into @'flatten' edBuf@.
    edCursor :: Int,
    -- | Selected field within the cursor row.
    edField :: Int,
    -- | One-line message under the panel (errors, save notes).
    edStatus :: String
  }

-- | Open a slot for editing. A spell outside the glyph subset opens as an
-- empty buffer (it stays untouched in the book unless committed over).
openEditor :: Int -> Spellbook -> EditorState
openEditor slot book = case decompile (slotSpell slot book) of
  Just buf -> EditorState slot buf 0 0 ""
  Nothing -> EditorState slot [] 0 0 "SLOT TOO INTRICATE FOR GLYPHS - STARTING EMPTY"

-- * Update

data EditorOut
  = EdContinue EditorState
  | -- | Compiled and within Willpower: write to this slot and close.
    EdCommit Int Stmt
  | EdCancel

paletteKeys :: [Scancode]
paletteKeys = [Scancode1, Scancode2, Scancode3, Scancode4, Scancode5, Scancode6]

updateEditor :: Input -> Spellbook -> EditorState -> EditorOut
updateEditor input book st
  | tapped ScancodeEscape || tapped ScancodeE = EdCancel
  | tapped ScancodeReturn = commit
  | tapped ScancodeTab = EdContinue switchSlot
  | tapped ScancodeUp = moveCursor (-1)
  | tapped ScancodeDown = moveCursor 1
  | tapped ScancodeLeft = moveField (-1)
  | tapped ScancodeRight = moveField 1
  | tapped ScancodeMinus = cycleCur (-1)
  | tapped ScancodeEquals = cycleCur 1
  | tapped ScancodeX || tapped ScancodeBackspace = deleteCur
  | Just i <- findIndex tapped paletteKeys = insertGlyph i
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

    switchSlot =
      (openEditor ((edSlot st + 1) `mod` slotCount) book)
        { edStatus = "SWITCHED SLOT - UNSAVED EDITS DISCARDED"
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
        withBuf (modifyAt (rowPath row) (cycleField scope (edField st) dir)) $
          \buf' -> st {edBuf = buf', edStatus = ""}

    deleteCur = case rowKind row of
      RHole -> EdContinue st {edStatus = "NOTHING TO UNSAY HERE"}
      RNode _ _ ->
        withBuf (deleteAt (rowPath row)) $ \buf' ->
          st
            { edBuf = buf',
              edCursor = max 0 (min (length (flatten buf') - 1) cur),
              edField = 0,
              edStatus = ""
            }

    insertGlyph i
      | glyphCount (edBuf st) >= willpowerMax =
          EdContinue st {edStatus = "WILLPOWER FULL"}
      | otherwise =
          let node = snd (paletteEntries !! i)
              -- On a hole, fill it; on a glyph, land beside it as a
              -- sibling — after by default, before with Shift.
              path = case (rowKind row, unsnoc (rowPath row)) of
                (RNode _ _, Just (prefix, j))
                  | not shiftHeld -> prefix ++ [j + 1]
                _ -> rowPath row
           in withBuf (insertAt path node) $ \buf' ->
                st
                  { edBuf = buf',
                    edCursor =
                      fromMaybe cur (findIndex ((== path) . rowPath) (flatten buf')),
                    edField = 0,
                    edStatus = ""
                  }

    withBuf edit k = case edit (edBuf st) of
      Just buf' -> EdContinue (k buf')
      Nothing -> EdContinue st {edStatus = "THE GLYPHS RESIST"} -- unreachable

-- * Drawing

titleColor, capColor, fieldColor, selColor, plainColor, holeColor :: Color
titleColor = Color 0.95 0.85 0.55 1
capColor = Color 0.95 0.4 0.3 1
fieldColor = Color 0.92 0.92 0.95 1
selColor = Color 1 0.9 0.3 1
plainColor = Color 0.62 0.62 0.7 1
holeColor = Color 0.45 0.45 0.52 1

rowH :: Int
rowH = 18

-- | The editor panel, drawn over the (frozen) world render.
drawEditor :: (MonadIO m) => Gfx -> EditorState -> m ()
drawEditor gfx st = do
  let V2 winW winH = gfxWinSize gfx
      buf = edBuf st
      rows = flatten buf
      cur = max 0 (min (length rows - 1) (edCursor st))
      cnt = glyphCount buf
      atCap = cnt >= willpowerMax

  fillUiRect gfx (V2 0 0) (V2 winW winH) (Color 0 0 0 0.62)

  drawText gfx TitleFont (V2 16 8) titleColor ("WYRDBOOK  SLOT " ++ show (edSlot st + 1))
  drawText
    gfx
    TitleFont
    (V2 (winW - 170) 8)
    (if atCap then capColor else titleColor)
    ("WILL " ++ show cnt ++ "/" ++ show willpowerMax)

  -- Palette (hotkeys 1-6); greyed once Willpower is spent.
  forM_ (zip [0 :: Int ..] paletteEntries) $ \(i, (label, _)) ->
    drawText
      gfx
      TextFont
      (V2 16 (48 + i * rowH))
      (if atCap then holeColor else fieldColor)
      (show (i + 1) ++ " " ++ label)

  -- The spell, one row per glyph (or hole), indented by nesting depth.
  forM_ (zip [0 :: Int ..] rows) $ \(i, r) -> do
    let y = 48 + i * rowH
        onCursor = i == cur
        x0 = 210 + 20 * rowDepth r
    forM_ [() | onCursor] $ \_ ->
      fillUiRect gfx (V2 200 (y - 2)) (V2 (winW - 216) rowH) (Color 0.25 0.28 0.45 0.9)
    let pieceColor mf = case mf of
          Nothing -> plainColor
          Just k
            | onCursor && k == edField st -> selColor
            | otherwise -> fieldColor
        drawPieces _ [] = pure ()
        drawPieces x ((mf, s) : ps) = do
          drawText gfx TextFont (V2 x y) (pieceColor mf) s
          V2 w _ <- measureText gfx TextFont s
          drawPieces (x + w + 8) ps
    case rowKind r of
      RHole -> drawText gfx TextFont (V2 x0 y) holeColor "( EMPTY )"
      RNode n _ -> drawPieces x0 (rowPieces n)

  drawText
    gfx
    TextFont
    (V2 16 (winH - 44))
    plainColor
    "ARROWS MOVE  -/= CHANGE  1-6 INSERT (SHIFT: BEFORE)  X DELETE  TAB SLOT  RET SAVE  ESC CANCEL"
  drawText gfx TextFont (V2 16 (winH - 24)) (Color 0.95 0.6 0.3 1) (edStatus st)

-- | A row's display pieces: 'Just' field pieces are Left\/Right-selectable
-- (the index matches 'cycleField'); 'Nothing' pieces are connectives.
rowPieces :: ENode -> [(Maybe Int, String)]
rowPieces n = case n of
  EInvoke v a ->
    (Just 0, verbT v)
      : [(Nothing, "AT") | v /= Push]
      ++ [(Just 1, argT a)]
  ERepeat k _ -> [(Nothing, "REPEAT"), (Just 0, show k), (Nothing, "TIMES")]
  EIf op t _ -> [(Nothing, "IF MANA"), (Just 0, opT op), (Just 1, show t)]
  ELet nm sel _ -> [(Nothing, "LET"), (Just 0, upcase nm), (Nothing, "="), (Just 1, selT sel)]
  where
    verbT v = case v of Bolt -> "BOLT"; Push -> "PUSH"; Kindle -> "KINDLE"
    selT s = case s of TileAhead -> "TILE AHEAD"; NearestFoe -> "NEAREST FOE"; SelfSel -> "SELF"
    argT a = case a of ASel s -> selT s; AVar nm -> upcase nm
    opT op = case op of
      Gt -> ">"
      Lt -> "<"
      Eq -> "="
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
    upcase = map toUpper
