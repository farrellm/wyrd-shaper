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

    -- * Edits
    insertAt,
    deleteAt,
    modifyAt,

    -- * Field editing
    fieldCount,
    cycleField,

    -- * To and from the Wyrdtongue
    compile,
    decompile,

    -- * Palette
    paletteEntries,
  )
where

import Data.List (elemIndex)
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

-- * Edits

-- | Run an indexed rewrite on the child list containing the path's target.
modList :: Path -> (Int -> [ENode] -> Maybe [ENode]) -> Spell -> Maybe Spell
modList [] _ _ = Nothing
modList [i] f ns = f i ns
modList (i : rest) f ns = case splitAt i ns of
  (before, n : after) -> do
    b' <- modList rest f (children n)
    n' <- withChildren n b'
    pure (before ++ n' : after)
  _ -> Nothing
  where
    children n = case n of
      EInvoke _ _ -> []
      ERepeat _ b -> b
      EIf _ _ b -> b
      ELet _ _ b -> b
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

-- | Step one field of a glyph forward (@dir = 1@) or back (@-1@): cycle
-- enumerated fields, clamp numeric ones. Out-of-range field indices leave
-- the glyph unchanged.
cycleField :: [Name] -> Int -> Int -> ENode -> ENode
cycleField scope field dir n = case (n, field) of
  (EInvoke v a, 0) -> EInvoke (cycleIn [Bolt, Push, Kindle] v) a
  (EInvoke v a, 1) -> EInvoke v (cycleIn args a)
    where
      args = map ASel [TileAhead, NearestFoe, SelfSel] ++ map AVar scope
  (ERepeat k b, 0) -> ERepeat (clampTo 1 9 (k + dir)) b
  (EIf op t b, 0) -> EIf (cycleIn [Gt, Lt, Eq] op) t b
  (EIf op t b, 1) -> EIf op (clampTo 0 manaMax (t + dir)) b
  (ELet nm sel b, 0) -> ELet (cycleIn letNames nm) sel b
  (ELet nm sel b, 1) -> ELet nm (cycleIn [NearestFoe, TileAhead, SelfSel] sel) b
  _ -> n
  where
    cycleIn :: (Eq a) => [a] -> a -> a
    cycleIn [] x = x
    cycleIn xs@(x0 : _) x = case elemIndex x xs of
      Just i -> xs !! ((i + dir) `mod` length xs)
      Nothing -> x0 -- e.g. a var that fell out of scope: restart the cycle
    clampTo lo hi = max lo . min hi

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
