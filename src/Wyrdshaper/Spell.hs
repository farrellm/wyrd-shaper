-- | The Wyrdtongue core: the spell AST and the ticked small-step interpreter.
--
-- Pure module (like "Wyrdshaper.Tilemap"): the ECS side hands 'step' a
-- 'WorldView' snapshot of the world and applies the 'Effect's it emits. One
-- 'step' call executes one instruction; the caller charges one mana per call
-- and paces calls in game ticks, which is what makes casting *channeling* —
-- a loop's bolts launch ticks apart, and long programs take long to speak.
--
-- What counts as an instruction: verbs and anything that evaluates an
-- expression ('If' conditions, 'Repeat' bounds, 'Let' bindings). Pure stack
-- bookkeeping ('Seq' unpacking, loop iteration, scope exit) is free, so a
-- loop that fires three bolts costs three bolts (plus its bound).
module Wyrdshaper.Spell
  ( -- * AST
    Name,
    FoeId,
    Value (..),
    Target (..),
    Selector (..),
    Op (..),
    Expr (..),
    Verb (..),
    Stmt (..),
    spellSize,

    -- * VM
    VM,
    newVM,
    WorldView (..),
    Effect (..),
    CastError (..),
    StepResult (..),
    step,

    -- * Geometry helpers
    velToward,

    -- * Tunables
    willpowerMax,
    manaMax,
    manaRegenTicks,
    ticksPerInstr,
    boltSpeed,
    boltTTL,
    boltDamage,
    dummyMaxHP,
    burnTicks,
    pushStrength,
    playerMaxHP,
    chaserHP,
    hexerHP,
    enemySpeed,
    chaserAggro,
    hexerAggro,
    contactDamage,
    contactCooldownTicks,
    enemyManaMax,
    enemyTicksPerInstr,
    hexerRecastTicks,
    staggerThreshold,
    backlashBase,
    backlashPerMana,
    backlashDamage,
    invulnTicks,
    hitFlashTicks,
    backlashFlashTicks,
  )
where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Linear (V2 (..), norm, quadrance)
import Wyrdshaper.Tilemap (tileCenter, tileSize)

-- * Tunables

-- Gameplay numbers live here as data, not scattered through code
-- (CONCEPT.md: balance will need sustained tweaking).

-- | Willpower caps 'spellSize' — how many glyphs the caster can hold in
-- mind at once. Enforced by the M3 editor (and defensively at cast time).
willpowerMax :: Int
willpowerMax = 8

manaMax, manaRegenTicks, ticksPerInstr :: Int
manaMax = 20
manaRegenTicks = 30 -- ticks per point of mana regained
ticksPerInstr = 6 -- ticks between VM instructions (10 instr/s at 60 tps)

boltSpeed, boltTTL, boltDamage :: Int
boltSpeed = 6 -- px per tick
boltTTL = 90 -- ticks before a bolt expires in flight
boltDamage = 1

dummyMaxHP, burnTicks, pushStrength :: Int
dummyMaxHP = 3
burnTicks = 300 -- how long a kindled tile stays alight
pushStrength = 48 -- px of shove

playerMaxHP, chaserHP, hexerHP :: Int
playerMaxHP = 10
chaserHP = 3
hexerHP = 2

enemySpeed, chaserAggro, hexerAggro :: Int
enemySpeed = 2 -- px per tick; slower than the player's 3
chaserAggro = 192 -- px (6 tiles); spawns must sit outside this of the start
hexerAggro = 224 -- px (7 tiles)

contactDamage, contactCooldownTicks :: Int
contactDamage = 1
contactCooldownTicks = 45 -- ticks between a chaser's contact hits

enemyManaMax, enemyTicksPerInstr, hexerRecastTicks :: Int
enemyManaMax = 6
enemyTicksPerInstr = 20 -- slow speech: a 3-instruction volley is a ~1s window
hexerRecastTicks = 150 -- ticks between a hexer's casts

-- | Minimum damage in one hit that staggers (interrupts) a channeling
-- caster. At 1, any hit staggers — player and enemy alike.
staggerThreshold :: Int
staggerThreshold = 1

backlashBase, backlashPerMana :: Int
backlashBase = 1
backlashPerMana = 2 -- mana committed per extra point of backlash

-- | Damage a collapsing spell deals its caster: a base bite plus one point
-- per 'backlashPerMana' mana already committed (the ECS charges one mana per
-- executed instruction, so instructions spent == mana committed). Big
-- spells, big risks.
backlashDamage :: Int -> Int
backlashDamage spent = backlashBase + spent `div` backlashPerMana

invulnTicks, hitFlashTicks, backlashFlashTicks :: Int
invulnTicks = 45 -- player i-frames after a hit
hitFlashTicks = 12 -- white blip on any hit
backlashFlashTicks = 30 -- longer wash when a cast collapses on its caster

-- * AST

type Name = String

-- | Opaque foe identity; the ECS side maps these to entity ids.
type FoeId = Int

data Value = VNum Int | VDir (V2 Int) | VTarget Target
  deriving (Eq, Show, Read)

-- | A resolved target. Foes are held by identity, not position: positions
-- are looked up again when a verb fires, so a 'Let'-bound foe that dies
-- mid-spell is a runtime error ('TargetGone'), not a stale hit.
data Target = TSelf | TFoe FoeId | TTile (V2 Int)
  deriving (Eq, Show, Read)

data Selector = SelfSel | NearestFoe | TileAhead
  deriving (Eq, Show, Read)

data Op = Add | Sub | Mul | Gt | Lt | Eq
  deriving (Eq, Show, Read)

data Expr
  = Lit Value
  | Var Name
  | Select Selector
  | ManaLeft
  | BinOp Op Expr Expr
  deriving (Eq, Show, Read)

data Verb = Bolt | Push | Kindle
  deriving (Eq, Show, Read)

data Stmt
  = Invoke Verb [Expr]
  | Seq [Stmt]
  | If Expr Stmt Stmt
  | Repeat Expr Stmt
  | Let Name Expr Stmt
  deriving (Eq, Show, Read)

-- | Static instruction count: the charged nodes of the program. ('Repeat'
-- bodies count once, so a running cast can exceed this — it is a size, not
-- a running time.) This is the measure Willpower will budget in M3.
spellSize :: Stmt -> Int
spellSize s = case s of
  Invoke _ _ -> 1
  Seq xs -> sum (map spellSize xs)
  If _ t f -> 1 + spellSize t + spellSize f
  Repeat _ b -> 1 + spellSize b
  Let _ _ b -> 1 + spellSize b

-- * VM

-- | What the interpreter can see of the world, rebuilt by the ECS side
-- before every 'step'.
data WorldView = WorldView
  { wvCaster :: V2 Int,
    -- | Unit-ish axis direction the caster faces (components in -1..1).
    wvFacing :: V2 Int,
    -- | Caster's mana after paying for the current instruction.
    wvMana :: Int,
    wvFoes :: [(FoeId, V2 Int)]
  }
  deriving (Show)

-- | What a step asks the world to do.
data Effect
  = -- | Origin and velocity (px\/tick) of a new bolt.
    SpawnBolt (V2 Int) (V2 Int)
  | -- | Shove a target by a pixel delta.
    PushEff Target (V2 Int)
  | -- | Set a tile (grid coordinates) alight.
    KindleEff (V2 Int)
  deriving (Eq, Show)

data CastError
  = NoTarget Selector
  | TargetGone FoeId
  | OutOfMana
  | BadSpell String
  deriving (Eq, Show)

-- | Continuation frames. 'FUnbind' closes a 'Let' scope; 'FLoop' holds the
-- remaining iterations of a 'Repeat'.
data Frame = FStmt Stmt | FLoop Int Stmt | FUnbind
  deriving (Show)

data VM = VM
  { vmFrames :: [Frame],
    vmEnv :: [(Name, Value)]
  }
  deriving (Show)

newVM :: Stmt -> VM
newVM s = VM [FStmt s] []

data StepResult
  = -- | The spell continues; effects (if any) from this instruction.
    Continue VM [Effect]
  | -- | The spell finished with this instruction.
    Done [Effect]
  | -- | Runtime error: the cast collapses and bites its caster for
    -- 'backlashDamage' of the mana already committed.
    Fizzle CastError
  deriving (Show)

-- | Execute one instruction.
step :: WorldView -> VM -> StepResult
step wv vm0 = case unwind vm0 of
  VM [] _ -> Done []
  VM (FStmt s : fs) env -> exec s fs env
  VM (FLoop _ _ : _) _ -> Fizzle (BadSpell "impossible: unwound loop head")
  VM (FUnbind : _) _ -> Fizzle (BadSpell "impossible: unwound scope head")
  where
    exec s fs env = case s of
      Invoke v args -> case traverse (evalExpr wv env) args >>= applyVerb wv v of
        Left err -> Fizzle err
        Right effs -> continue (VM fs env) effs
      Seq _ -> Fizzle (BadSpell "impossible: Seq is unwound")
      If c t f -> case evalExpr wv env c of
        Left err -> Fizzle err
        Right (VNum n) -> continue (VM (FStmt (if n /= 0 then t else f) : fs) env) []
        Right _ -> Fizzle (BadSpell "if condition is not a number")
      Repeat n b -> case evalExpr wv env n of
        Left err -> Fizzle err
        Right (VNum k)
          | spellSize b <= 0 -> Fizzle (BadSpell "empty loop body")
          | otherwise -> continue (VM (FLoop k b : fs) env) []
        Right _ -> Fizzle (BadSpell "repeat count is not a number")
      Let nm e body -> case evalExpr wv env e of
        Left err -> Fizzle err
        Right v -> continue (VM (FStmt body : FUnbind : fs) ((nm, v) : env)) []

    continue vm effs = case unwind vm of
      VM [] _ -> Done effs
      vm' -> Continue vm' effs

-- | Process free bookkeeping at the head of the stack until it starts with
-- a chargeable statement (or is empty). Terminates: 'Seq' unpacking shrinks
-- the program, and 'Repeat' bodies are checked non-empty at loop entry.
unwind :: VM -> VM
unwind vm = case vm of
  VM (FUnbind : fs) env -> unwind (VM fs (drop 1 env))
  VM (FLoop k body : fs) env
    | k <= 0 -> unwind (VM fs env)
    | otherwise -> unwind (VM (FStmt body : FLoop (k - 1) body : fs) env)
  VM (FStmt (Seq xs) : fs) env -> unwind (VM (map FStmt xs ++ fs) env)
  _ -> vm

-- * Evaluation

evalExpr :: WorldView -> [(Name, Value)] -> Expr -> Either CastError Value
evalExpr wv env e = case e of
  Lit v -> Right v
  Var n -> maybe (Left (BadSpell ("unbound name " ++ show n))) Right (lookup n env)
  Select sel -> VTarget <$> selectTarget wv sel
  ManaLeft -> Right (VNum (wvMana wv))
  BinOp op a b -> do
    va <- evalExpr wv env a
    vb <- evalExpr wv env b
    case (va, vb) of
      (VNum x, VNum y) ->
        Right . VNum $ case op of
          Add -> x + y
          Sub -> x - y
          Mul -> x * y
          Gt -> fromEnum (x > y)
          Lt -> fromEnum (x < y)
          Eq -> fromEnum (x == y)
      _ -> Left (BadSpell "arithmetic on non-numbers")

selectTarget :: WorldView -> Selector -> Either CastError Target
selectTarget wv sel = case sel of
  SelfSel -> Right TSelf
  TileAhead ->
    Right . TTile . tileOf $ wvCaster wv + fmap (* tileSize) (wvFacing wv)
  NearestFoe -> case wvFoes wv of
    [] -> Left (NoTarget sel)
    fs -> Right . TFoe . fst $ minimumBy (comparing (qdTo . snd)) fs
      where
        qdTo p = quadrance (p - wvCaster wv)

applyVerb :: WorldView -> Verb -> [Value] -> Either CastError [Effect]
applyVerb wv v args = case (v, args) of
  (Bolt, [VDir d])
    | d == V2 0 0 -> Left (BadSpell "bolt with no direction")
    | otherwise -> Right [SpawnBolt (wvCaster wv) (velToward boltSpeed (V2 0 0) d)]
  (Bolt, [arg]) -> do
    aim <- targetPoint wv arg
    let vel = velToward boltSpeed (wvCaster wv) aim
    if vel == V2 0 0
      then Left (BadSpell "bolt with no direction")
      else Right [SpawnBolt (wvCaster wv) vel]
  (Push, [arg@(VTarget t)]) -> do
    p <- targetPoint wv arg
    Right [PushEff t (velToward pushStrength (wvCaster wv) p)]
  (Kindle, [arg]) -> do
    p <- targetPoint wv arg
    Right [KindleEff (tileOf p)]
  _ -> Left (BadSpell ("bad arguments to " ++ show v))

-- | Where a target value is right now, per the current 'WorldView'.
targetPoint :: WorldView -> Value -> Either CastError (V2 Int)
targetPoint wv v = case v of
  VTarget TSelf -> Right (wvCaster wv)
  VTarget (TFoe fid) ->
    maybe (Left (TargetGone fid)) Right (lookup fid (wvFoes wv))
  VTarget (TTile txy) -> Right (tileCenter txy)
  _ -> Left (BadSpell "expected a target")

-- | A vector of the given magnitude from one point toward another (zero if
-- they coincide).
velToward :: Int -> V2 Int -> V2 Int -> V2 Int
velToward mag from to =
  let d = fmap fromIntegral (to - from) :: V2 Double
      len = norm d
   in if len < 0.5
        then V2 0 0
        else fmap (round . (* (fromIntegral mag / len))) d

-- | Grid coordinates of the tile containing a world-pixel point.
tileOf :: V2 Int -> V2 Int
tileOf = fmap (`div` tileSize)
