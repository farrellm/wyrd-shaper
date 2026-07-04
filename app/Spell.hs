module Spell
  ( Verb (..),
    Selector (..),
    Instr (..),
    Program,
    manaCost,
    tripleBoltSpell,
    pushSpell,
    kindleSpell,
    quickSlotProgram,
    resolveSelector,
    addTile,
  )
where

import Aztecs.GLFW (Key (..))

-- | Effect verbs. Only the ones M2 needs a hardcoded spell for exist yet --
-- 'shield', 'douse', 'mend', 'bind', 'hush' etc. from CONCEPT.md's Wyrtongue
-- arrive with the milestones that give them something to act on.
data Verb = Bolt | Push | Kindle
  deriving (Eq, Show)

-- | Expressions that resolve to a target tile at cast time. Only "tile
-- ahead" is resolvable pre-M4: there are no enemies yet for "nearest foe" or
-- "self" (self doesn't need a selector for any of Bolt/Push/Kindle) to mean
-- anything.
data Selector = AheadSel
  deriving (Eq, Show)

-- | A single spell instruction: apply a verb to whatever a selector
-- resolves to. This is deliberately the entire core AST for M2 -- no
-- 'If'/'Loop'/'Let'/parameters. CONCEPT.md's progression table gates those
-- behind later tiers (bounded loops at Tier 3, named/parameterized spells at
-- Tier 5+), and hardcoded Haskell-level 'Program's don't need them to
-- satisfy M2's "done when". Don't mistake the narrowness for an oversight --
-- growing this type is M3 (block editor) and M7 (text language)'s job.
data Instr = Cast Verb Selector
  deriving (Eq, Show)

-- | A spell is a flat sequence of instructions, run one per interpreter
-- step (the ticked coroutine in "Main.hs" spaces steps several ticks apart
-- so the sequencing is visible, not just technically true).
type Program = [Instr]

-- | Mana cost per instruction *executed*, not per spell cast -- per
-- CONCEPT.md, a three-bolt loop costs three bolts. Placeholder numbers,
-- tune during playtesting.
manaCost :: Verb -> Int
manaCost Bolt = 3
manaCost Push = 4
manaCost Kindle = 5

-- | The three hardcoded quick-slot spells M2 ships with. 'tripleBoltSpell'
-- is the one that demonstrates multi-instruction channeling over several
-- ticks; the other two each demonstrate one of the other verbs.
tripleBoltSpell, pushSpell, kindleSpell :: Program
tripleBoltSpell = replicate 3 (Cast Bolt AheadSel)
pushSpell = [Cast Push AheadSel]
kindleSpell = [Cast Kindle AheadSel]

-- | Quick-slot key bindings. There's no in-game editor yet (that's M3), so
-- these are just hardcoded Haskell values bound to number keys.
quickSlotProgram :: Key -> Maybe Program
quickSlotProgram Key'1 = Just tripleBoltSpell
quickSlotProgram Key'2 = Just pushSpell
quickSlotProgram Key'3 = Just kindleSpell
quickSlotProgram _ = Nothing

-- | Add a tile-space delta to a tile-space position.
addTile :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTile (c1, r1) (c2, r2) = (c1 + c2, r1 + r2)

-- | Resolve a selector to the tile it targets, given the caster's tile
-- position and facing (as a tile-space unit delta). Pure for now because
-- 'AheadSel' happens to need no world state beyond position/facing; once
-- entity-based selectors ("nearest foe") exist this will need to move into
-- the impure interpreter step, where it can query the world.
resolveSelector :: Selector -> (Int, Int) -> (Int, Int) -> (Int, Int)
resolveSelector AheadSel playerTile facingDelta = playerTile `addTile` facingDelta
