-- | The player's quick-slot spellbook: the default spells and disk
-- persistence.
--
-- The save format is one header line followed by one derived-'Show'n 'Stmt'
-- per slot per line ('Read' parses it back). Loading is forgiving per slot:
-- a missing file, bad header, unparsable line, or a spell over the Willpower
-- budget falls back to that slot's default with a warning on stderr — a
-- broken save never blocks the game from starting.
module Wyrdshaper.Spellbook
  ( Spellbook (..),
    slotCount,
    defaultSpellbook,
    slotSpell,
    setSlot,
    spellbookPath,
    saveSpellbook,
    loadSpellbook,

    -- * Default spells
    fireboltSpell,
    volleySpell,
    brandRepelSpell,

    -- * Enemy spells
    hexerSpell,
  )
where

import Control.Exception (IOException, try)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Wyrdshaper.Spell

-- | The quick-slot spells; always exactly 'slotCount' entries.
newtype Spellbook = Spellbook {sbSlots :: [Stmt]}
  deriving (Eq, Show)

slotCount :: Int
slotCount = 3

-- | Slot 1: a bolt at the tile you face.
fireboltSpell :: Stmt
fireboltSpell = Invoke Bolt [Select TileAhead]

-- | Slot 2: three bolts at the nearest dummy, re-aimed each iteration,
-- launching ticks apart.
volleySpell :: Stmt
volleySpell = Repeat (Lit (VNum 3)) (Invoke Bolt [Select NearestFoe])

-- | Slot 3: bind the nearest dummy, shove it if mana allows, and kindle the
-- tile ahead — let\/seq\/if in one spell.
brandRepelSpell :: Stmt
brandRepelSpell =
  Let "t" (Select NearestFoe) $
    Seq
      [ If
          (BinOp Gt ManaLeft (Lit (VNum 2)))
          (Invoke Push [Var "t"])
          (Seq []),
        Invoke Kindle [Select TileAhead]
      ]

-- | What a hexer channels: a two-bolt volley at its nearest foe (the
-- player). Three instructions of slow enemy speech — a real window to
-- interrupt. If the target vanishes mid-channel, 'NearestFoe' fizzles and
-- the hexer eats its own backlash, same rules as the player.
hexerSpell :: Stmt
hexerSpell = Repeat (Lit (VNum 2)) (Invoke Bolt [Select NearestFoe])

defaultSpellbook :: Spellbook
defaultSpellbook = Spellbook [fireboltSpell, volleySpell, brandRepelSpell]

-- | The spell in a slot (0-based); out-of-range asks are a caller bug.
slotSpell :: Int -> Spellbook -> Stmt
slotSpell i (Spellbook ss) = ss !! i

setSlot :: Int -> Stmt -> Spellbook -> Spellbook
setSlot i s (Spellbook ss) =
  Spellbook [if j == i then s else old | (j, old) <- zip [0 ..] ss]

spellbookPath :: FilePath
spellbookPath = "spellbook.wyrd"

header :: String
header = "wyrdshaper-spellbook v1"

saveSpellbook :: FilePath -> Spellbook -> IO ()
saveSpellbook path (Spellbook ss) =
  writeFile path (unlines (header : map show ss))

loadSpellbook :: FilePath -> IO Spellbook
loadSpellbook path = do
  eContents <- try (readFile path)
  case eContents of
    Left (e :: IOException) -> do
      warn ("no spellbook at " ++ path ++ " (" ++ show e ++ "); using defaults")
      pure defaultSpellbook
    Right contents -> case lines contents of
      (h : rest)
        | h == header ->
            Spellbook <$> mapM (uncurry restore) (zip [0 ..] (pad rest))
      _ -> do
        warn (path ++ " has an unrecognized header; using defaults")
        pure defaultSpellbook
  where
    pad rest = take slotCount (rest ++ repeat "")
    restore i line = case readMaybe line of
      Just s
        | spellSize s <= willpowerMax -> pure s
        | otherwise -> fallBack i ("slot " ++ show (i + 1) ++ " exceeds Willpower")
      Nothing -> fallBack i ("slot " ++ show (i + 1) ++ " does not parse")
    fallBack i why = do
      warn (path ++ ": " ++ why ++ "; using that slot's default")
      pure (slotSpell i defaultSpellbook)
    warn = hPutStrLn stderr . ("spellbook: " ++)
