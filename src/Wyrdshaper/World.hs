{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | The apecs world: component types, their stores, and the world splice.
module Wyrdshaper.World
  ( -- * Components
    Position (..),
    Mana (..),
    Facing (..),
    Casting (..),
    CastState (..),
    Projectile (..),
    Health (..),
    Burning (..),
    Faction (..),
    EnemyKind (..),
    Enemy (..),
    HitFlash (..),
    Invuln (..),
    Torch (..),
    Anim (..),
    Corpse (..),

    -- * World
    World,
    System',
    initWorld,
    destroyEntity,

    -- * Game context
    Place (..),
    Level (..),
    Game (..),
  )
where

import Apecs
import Data.IORef (IORef)
import Linear (V2)
import Wyrdshaper.Spell (VM)
import Wyrdshaper.Tilemap (Tilemap)
import Wyrdshaper.Worldgen (Dungeon, Overworld)

-- | An entity's pixel position: the center of its AABB, origin bottom-left,
-- y up (the world convention; the flip to SDL screen space happens in
-- "Wyrdshaper.Engine").
newtype Position = Position (V2 Int)

instance Component Position where type Storage Position = Map Position

-- | Current mana, this caster's cap, and the regen clock (ticks since the
-- last point back). On the player and on enemy casters.
data Mana = Mana !Int !Int !Int

instance Component Mana where type Storage Mana = Map Mana

-- | Last nonzero movement direction (components in -1..1); aims 'TileAhead'
-- and slot-1 bolts. On the player and on enemy casters.
newtype Facing = Facing (V2 Int)

instance Component Facing where type Storage Facing = Map Facing

-- | Present on a caster while channeling a spell (which roots movement);
-- absent when idle. Player and enemy casters channel under the same rules.
newtype Casting = Casting CastState

instance Component Casting where type Storage Casting = Map Casting

data CastState = CastState
  { castVM :: VM,
    -- | Ticks until the next VM instruction.
    castCooldown :: !Int,
    -- | Instructions executed so far (HUD numerator; == mana committed).
    castSpent :: !Int,
    -- | 'Wyrdshaper.Spell.spellSize' of the program (HUD denominator).
    castSize :: !Int,
    -- | Ticks between VM instructions for this caster (the player speaks
    -- faster than a hexer; same rules, tunable tempo).
    castPace :: !Int
  }

-- | A bolt in flight: velocity (px\/tick) and remaining ticks to live.
-- Bolts also carry their caster's 'Faction' — they only hit the other side.
data Projectile = Projectile !(V2 Int) !Int

instance Component Projectile where type Storage Projectile = Map Projectile

-- | A combatant's current and maximum hit points. On the player, enemies,
-- and target dummies alike.
data Health = Health !Int !Int

instance Component Health where type Storage Health = Map Health

-- | A fire overlay: the tile alight and ticks of burn left. Burn entities
-- carry no 'Position'; the tile coordinate is the position.
data Burning = Burning !(V2 Int) !Int

instance Component Burning where type Storage Burning = Map Burning

-- | Whose side an entity fights on; decides who hits whom. On combatants
-- and on their bolts.
data Faction = FPlayer | FEnemy
  deriving (Eq)

instance Component Faction where type Storage Faction = Map Faction

data EnemyKind = Dummy | Chaser | Hexer
  deriving (Eq)

-- | An enemy: its kind and its action cooldown (ticks until the next
-- contact hit for a 'Chaser', the next cast for a 'Hexer').
data Enemy = Enemy !EnemyKind !Int

instance Component Enemy where type Storage Enemy = Map Enemy

-- | Ticks of hit-flash left; entities flash white when hurt, and the
-- player's flash also drives the full-screen damage wash.
newtype HitFlash = HitFlash Int

instance Component HitFlash where type Storage HitFlash = Map HitFlash

-- | Player i-frames: ticks of post-hit invulnerability left.
newtype Invuln = Invuln Int

instance Component Invuln where type Storage Invuln = Map Invuln

-- | A dungeon torch: 0 is unlit, otherwise ticks of flame left. Torch
-- entities carry a 'Position' (their tile's center), so level teardowns
-- sweep them up with everything else.
newtype Torch = Torch Int

instance Component Torch where type Storage Torch = Map Torch

-- | Sprite-animation state: the owner's position last tick, a free-running
-- tick clock (frame phase), and whether the owner moved this tick — set by
-- comparing positions, so every movement source (keys, snap glide, shoves)
-- counts.
data Anim = Anim !(V2 Int) !Int !Bool

instance Component Anim where type Storage Anim = Map Anim

-- | A slain monster's departing ghost — purely visual. Carries what the
-- die-sheet frame needs (the victim's kind, facing, and variant salt) and
-- the ticks of rise-and-fade left. Deliberately no 'Faction', 'Enemy', or
-- 'Health': targeting, bolts, and AI must never see a corpse.
data Corpse = Corpse !EnemyKind !(V2 Int) !Int !Int

instance Component Corpse where type Storage Corpse = Map Corpse

makeWorld
  "World"
  [ ''Position,
    ''Mana,
    ''Facing,
    ''Casting,
    ''Projectile,
    ''Health,
    ''Burning,
    ''Faction,
    ''Enemy,
    ''HitFlash,
    ''Invuln,
    ''Torch,
    ''Anim,
    ''Corpse
  ]

type System' a = SystemT World IO a

-- | Every component in the world. 'destroy' only removes the components
-- named in its Proxy, so full deletion must name them all; keep this in
-- sync with the 'makeWorld' list above (nested because apecs tuple
-- instances stop at 8 elements).
type AllComponents =
  ( Position,
    Mana,
    Facing,
    Casting,
    Projectile,
    Health,
    (Burning, Faction, Enemy, HitFlash, Invuln, Torch, Anim, Corpse)
  )

-- | Fully delete an entity. The only way game code should despawn anything.
destroyEntity :: Entity -> System' ()
destroyEntity e = destroy e (Proxy @AllComponents)

data Place = InOverworld | InDungeon
  deriving (Eq)

-- | The level the player is standing in: which place, its tilemap (a
-- mutable copy of the generated one — the puzzle door is unlocked by
-- rewriting a tile here), and the dungeon's puzzle latches. Entering a
-- level resets this from the cached generation results, so re-entry
-- regenerates-from-seed for free (and re-locks the door — an accepted M5
-- simplification).
data Level = Level
  { lvPlace :: Place,
    lvMap :: Tilemap,
    lvDoorOpen :: Bool,
    lvGoalDone :: Bool
  }

-- | Everything the systems need each tick beyond the world itself. The
-- generated overworld and dungeon are immutable per run; only 'gameLevel'
-- changes (owned by the loop closures, like the 'Shell').
data Game = Game
  { gamePlayer :: Entity,
    gameOverworld :: Overworld,
    gameDungeon :: Dungeon,
    gameLevel :: IORef Level
  }
