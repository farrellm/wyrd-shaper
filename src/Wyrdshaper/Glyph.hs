-- | The glyph-editable subset of the Wyrdtongue and the block editor's
-- document model (M3). Pure and repl-testable, like "Wyrdshaper.Spell".
--
-- The subset is chosen so that every block node has exactly /one/ child
-- list ('EIf' is then-only; its else compiles to @Seq []@). That makes a
-- cursor path a plain list of child indices — no zipper needed — while
-- still expressing all three default spells.
module Wyrdshaper.Glyph
  ( -- * The glyph subset
    ENode (..),
    Arg (..),
    Spell,
    glyphCount,

    -- * Rows and paths
    Path,
    Row (..),
    RowKind (..),
    flatten,

    -- * Insertion points (drag\/snap targets)
    InsPoint (..),
    insertionPoints,

    -- * Edits
    nodeAt,
    insertAt,
    deleteAt,
    modifyAt,
    moveNode,

    -- * Field editing
    fieldCount,
    fieldOptions,
    cycleField,

    -- * Display text
    rowPieces,
    verbText,
    selText,
    argText,
    opText,

    -- * To and from the Wyrdtongue
    compile,
    decompile,

    -- * Palette
    paletteEntries,
  )
where

import Data.Char (toUpper)
import Data.List (findIndex, unsnoc)
import Data.Maybe (fromMaybe)
import Wyrdshaper.Spell
  ( Expr (..),
    Name,
    Op (..),
    Selector (..),
    Stmt (..),
    Value (..),
    Verb (..),
    manaMax,
  )

-- * The glyph subset

-- | A verb argument: a selector glyph or a 'ELet'-bound name.
data Arg = ASel Selector | AVar Name
  deriving (Eq, Show)

-- | One glyph block. Each costs 1 glyph of Willpower plus its children;
-- sequencing is implicit in the child lists (and the top-level 'Spell'),
-- matching 'Wyrdshaper.Spell.spellSize' where 'Seq' is free.
data ENode
  = EInvoke Verb Arg
  | ERepeat Int [ENode]
  | EIf Op Int [ENode] -- ^ condition is @mana \<op\> n@
  | ELet Name Selector [ENode]
  deriving (Eq, Show)

-- | An editor document: the spell's top-level sequence.
type Spell = [ENode]

-- | Willpower cost of the document; mirrors 'Wyrdshaper.Spell.spellSize'
-- of the compiled spell (holes are free).
glyphCount :: Spell -> Int
glyphCount = sum . map nodeCount
  where
    nodeCount n = case n of
      EInvoke _ _ -> 1
      ERepeat _ b -> 1 + glyphCount b
      EIf _ _ b -> 1 + glyphCount b
      ELet _ _ b -> 1 + glyphCount b

-- * Rows and paths

-- | Path from the root: the head indexes the top-level list, each further
-- element indexes the child list of the node above. Unambiguous because
-- every block node has exactly one child list.
type Path = [Int]

-- | One display line of the editor: a glyph, or the hole shown inside an
-- empty child list (and for the empty spell) so the cursor has somewhere
-- to sit and inserts have a place to land.
data RowKind
  = -- | A glyph and the 'ELet' names in scope at it (for 'AVar' cycling).
    RNode ENode [Name]
  | RHole
  deriving (Eq, Show)

data Row = Row
  { rowPath :: Path,
    rowDepth :: Int,
    rowKind :: RowKind
  }
  deriving (Eq, Show)

-- | Preorder flattening; the editor cursor is an index into this list.
-- Every empty child list contributes an 'RHole' at its first index.
flatten :: Spell -> [Row]
flatten = goList [] 0 []
  where
    goList path depth _ [] = [Row (path ++ [0]) depth RHole]
    goList path depth scope ns = concat (zipWith one [0 ..] ns)
      where
        one i n =
          let p = path ++ [i]
              rest scope' b = goList p (depth + 1) scope' b
           in Row p depth (RNode n scope) : case n of
                EInvoke _ _ -> []
                ERepeat _ b -> rest scope b
                EIf _ _ b -> rest scope b
                ELet nm _ b -> rest (scope ++ [nm]) b

-- * Insertion points

-- | One place 'insertAt' may put a glyph: its path, the flat row index it
-- sits above (@length (flatten sp)@ for the very end), and its depth. A
-- hole's point /is/ its row ('ipAtHole') — the editor highlights the row
-- rather than drawing a gap line. Several end-of-body points can share one
-- 'ipBeforeRow'; they differ in 'ipDepth', which is how the drop pick
-- disambiguates (Scratch's "how far right is the pointer").
data InsPoint = InsPoint
  { ipPath :: Path,
    ipBeforeRow :: Int,
    ipDepth :: Int,
    ipAtHole :: Bool
  }
  deriving (Eq, Show)

-- | Every insertion point of the document, in row order; the snap targets
-- for a drag. Each child list of n children yields n+1 points; each empty
-- list yields its single hole point.
insertionPoints :: Spell -> [InsPoint]
insertionPoints = snd . goL [] 0 0
  where
    -- returns (next flat row index, points for this list and below)
    goL path depth i [] = (i + 1, [InsPoint (path ++ [0]) i depth True])
    goL path depth i0 ns = go i0 0 ns
      where
        go i j [] = (i, [InsPoint (path ++ [j]) i depth False])
        go i j (n : rest) =
          let pt = InsPoint (path ++ [j]) i depth False
              (i1, sub) = case n of
                EInvoke _ _ -> (i + 1, [])
                ERepeat _ b -> goL (path ++ [j]) (depth + 1) (i + 1) b
                EIf _ _ b -> goL (path ++ [j]) (depth + 1) (i + 1) b
                ELet _ _ b -> goL (path ++ [j]) (depth + 1) (i + 1) b
              (i2, more) = go i1 (j + 1) rest
           in (i2, pt : sub ++ more)

-- * Edits

nodeChildren :: ENode -> [ENode]
nodeChildren n = case n of
  EInvoke _ _ -> []
  ERepeat _ b -> b
  EIf _ _ b -> b
  ELet _ _ b -> b

-- | The node at a path, if the path names one.
nodeAt :: Path -> Spell -> Maybe ENode
nodeAt p sp = case p of
  [] -> Nothing
  [i] -> ix i sp
  (i : rest) -> nodeAt rest . nodeChildren =<< ix i sp
  where
    ix i ns
      | i >= 0, n : _ <- drop i ns = Just n
      | otherwise = Nothing

-- | Run an indexed rewrite on the child list containing the path's target.
modList :: Path -> (Int -> [ENode] -> Maybe [ENode]) -> Spell -> Maybe Spell
modList [] _ _ = Nothing
modList [i] f ns = f i ns
modList (i : rest) f ns = case splitAt i ns of
  (before, n : after) -> do
    b' <- modList rest f (nodeChildren n)
    n' <- withChildren n b'
    pure (before ++ n' : after)
  _ -> Nothing
  where
    withChildren n b = case n of
      EInvoke _ _ -> Nothing -- verbs have no child list to descend into
      ERepeat k _ -> Just (ERepeat k b)
      EIf op t _ -> Just (EIf op t b)
      ELet nm sel _ -> Just (ELet nm sel b)

-- | Insert so the new glyph ends up /at/ the path's final index.
insertAt :: Path -> ENode -> Spell -> Maybe Spell
insertAt p new = modList p $ \i ns ->
  if i < 0 || i > length ns
    then Nothing
    else Just (take i ns ++ new : drop i ns)

-- | Remove the whole subtree at the path.
deleteAt :: Path -> Spell -> Maybe Spell
deleteAt p = modList p $ \i ns ->
  if i < 0 || i >= length ns
    then Nothing
    else Just (take i ns ++ drop (i + 1) ns)

modifyAt :: Path -> (ENode -> ENode) -> Spell -> Maybe Spell
modifyAt p f = modList p $ \i ns -> case splitAt i ns of
  (before, n : after) -> Just (before ++ f n : after)
  _ -> Nothing

-- | Move the subtree at @src@ so it lands at insertion point @dst@, where
-- @dst@ is expressed against the /pre-move/ document (as enumerated by
-- 'insertionPoints'). Returns the landed path (so a cursor can follow) and
-- the new document. 'Nothing' when @dst@ is inside @src@'s own subtree.
--
-- The delete happens first, so a destination that passes through @src@'s
-- own list at a later index must shift down by one. Dropping on either gap
-- adjacent to @src@ needs no special case: both resolve to reinserting at
-- the original index — a no-op.
moveNode :: Path -> Path -> Spell -> Maybe (Path, Spell)
moveNode src dst sp = do
  (srcInit, srcLast) <- unsnoc src
  let k = length src
      intoOwnSubtree = length dst > k && take k dst == src
      adjust d
        | take (k - 1) d == srcInit,
          j : rest <- drop (k - 1) d,
          j > srcLast =
            take (k - 1) d ++ (j - 1) : rest
        | otherwise = d
  if intoOwnSubtree
    then Nothing
    else do
      n <- nodeAt src sp
      sp' <- deleteAt src sp
      let dst' = adjust dst
      sp'' <- insertAt dst' n sp'
      pure (dst', sp'')

-- * Field editing

-- | How many Left\/Right-selectable fields a glyph has.
fieldCount :: ENode -> Int
fieldCount n = case n of
  EInvoke _ _ -> 2 -- verb, argument
  ERepeat _ _ -> 1 -- count
  EIf _ _ _ -> 2 -- comparison, threshold
  ELet _ _ _ -> 2 -- name, selector

-- | Names an 'ELet' may bind.
letNames :: [Name]
letNames = ["t", "u", "v"]

-- | Every value one field of a glyph may take: display label plus the
-- glyph with that value set. The single source of truth for the editor's
-- dropdown menus and keyboard cycling alike — the current value is the
-- option whose node equals the input. Out-of-range fields have no options.
fieldOptions :: [Name] -> Int -> ENode -> [(String, ENode)]
fieldOptions scope field n = case (n, field) of
  (EInvoke _ a, 0) -> [(verbText v, EInvoke v a) | v <- [Bolt, Push, Kindle]]
  (EInvoke v _, 1) ->
    [ (argText a, EInvoke v a)
      | a <- map ASel [TileAhead, NearestFoe, SelfSel] ++ map AVar scope
    ]
  (ERepeat _ b, 0) -> [(show k, ERepeat k b) | k <- [1 .. 9]]
  (EIf _ t b, 0) -> [(opText op, EIf op t b) | op <- [Gt, Lt, Eq]]
  (EIf op _ b, 1) -> [(show t, EIf op t b) | t <- [0 .. manaMax]]
  (ELet _ sel b, 0) -> [(map toUpper nm, ELet nm sel b) | nm <- letNames]
  (ELet nm _ b, 1) -> [(selText s, ELet nm s b) | s <- [NearestFoe, TileAhead, SelfSel]]
  _ -> []

-- | Step one field forward (@dir = 1@) or back (@-1@) through its
-- 'fieldOptions', wrapping at the ends. An unrecognized current value
-- (e.g. a var that fell out of scope) restarts at the first option;
-- out-of-range field indices leave the glyph unchanged.
cycleField :: [Name] -> Int -> Int -> ENode -> ENode
cycleField scope field dir n = case fieldOptions scope field n of
  [] -> n
  opts -> snd (opts !! ((i + dir) `mod` length opts))
    where
      i = fromMaybe (-dir) (findIndex ((== n) . snd) opts)

-- * To and from the Wyrdtongue

-- | Compile the document to a castable 'Stmt'. Rejects what the VM (or the
-- caster) cannot survive being saved: an empty spell, a 'ERepeat' with an
-- empty body (a guaranteed fizzle), and arguments naming out-of-scope
-- variables. Single-child lists compile unwrapped so the defaults
-- round-trip exactly.
compile :: Spell -> Either String Stmt
compile [] = Left "THE SPELL IS EMPTY"
compile ns = body [] ns

body :: [Name] -> [ENode] -> Either String Stmt
body scope ns = do
  ss <- mapM (node scope) ns
  pure $ case ss of
    [s] -> s
    _ -> Seq ss
  where
    node sc n = case n of
      EInvoke v a -> do
        e <- argExpr sc a
        pure (Invoke v [e])
      ERepeat k b
        | null b -> Left "REPEAT HAS AN EMPTY BODY"
        | otherwise -> Repeat (Lit (VNum k)) <$> body sc b
      EIf op t b ->
        If (BinOp op ManaLeft (Lit (VNum t))) <$> body sc b <*> pure (Seq [])
      ELet nm sel b -> Let nm (Select sel) <$> body (sc ++ [nm]) b
    argExpr sc a = case a of
      ASel sel -> Right (Select sel)
      AVar nm
        | nm `elem` sc -> Right (Var nm)
        | otherwise -> Left ("NAME " ++ nm ++ " IS NOT BOUND HERE")

-- | Partial inverse of 'compile': 'Nothing' for spells outside the glyph
-- subset (an 'If' with a non-empty else, a computed 'Repeat' bound, …).
decompile :: Stmt -> Maybe Spell
decompile = deList
  where
    deList s = case s of
      Seq xs -> concat <$> mapM deList xs
      _ -> (: []) <$> deNode s
    deNode s = case s of
      Invoke v [e] -> EInvoke v <$> deArg e
      Repeat (Lit (VNum k)) b -> ERepeat k <$> deList b
      If (BinOp op ManaLeft (Lit (VNum t))) thn (Seq []) ->
        EIf op t <$> deList thn
      Let nm (Select sel) b -> ELet nm sel <$> deList b
      _ -> Nothing
    deArg e = case e of
      Select sel -> Just (ASel sel)
      Var nm -> Just (AVar nm)
      _ -> Nothing

-- * Palette

-- | The insertable glyphs, in the order of their editor hotkeys (1–6),
-- each with sensible default fields. Every entry costs exactly 1 glyph on
-- insert (blocks arrive with an empty body).
paletteEntries :: [(String, ENode)]
paletteEntries =
  [ ("BOLT", EInvoke Bolt (ASel TileAhead)),
    ("PUSH", EInvoke Push (ASel NearestFoe)),
    ("KINDLE", EInvoke Kindle (ASel TileAhead)),
    ("REPEAT", ERepeat 2 []),
    ("IF", EIf Gt 2 []),
    ("LET", ELet "t" NearestFoe [])
  ]

-- * Display text

-- | A row's display pieces: 'Just' field pieces are selectable (the index
-- matches 'fieldOptions'\/'cycleField' field numbering); 'Nothing' pieces
-- are connectives. Field labels come from the same helpers 'fieldOptions'
-- uses, so a menu's entries always read like the row itself.
rowPieces :: ENode -> [(Maybe Int, String)]
rowPieces n = case n of
  EInvoke v a ->
    (Just 0, verbText v)
      : [(Nothing, "AT") | v /= Push]
      ++ [(Just 1, argText a)]
  ERepeat k _ -> [(Nothing, "REPEAT"), (Just 0, show k), (Nothing, "TIMES")]
  EIf op t _ -> [(Nothing, "IF MANA"), (Just 0, opText op), (Just 1, show t)]
  ELet nm sel _ ->
    [(Nothing, "LET"), (Just 0, map toUpper nm), (Nothing, "="), (Just 1, selText sel)]

verbText :: Verb -> String
verbText v = case v of Bolt -> "BOLT"; Push -> "PUSH"; Kindle -> "KINDLE"

selText :: Selector -> String
selText s = case s of TileAhead -> "TILE AHEAD"; NearestFoe -> "NEAREST FOE"; SelfSel -> "SELF"

argText :: Arg -> String
argText a = case a of ASel s -> selText s; AVar nm -> map toUpper nm

opText :: Op -> String
opText op = case op of
  Gt -> ">"
  Lt -> "<"
  Eq -> "="
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
