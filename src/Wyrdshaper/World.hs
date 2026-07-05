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
    DummyHP (..),
    Burning (..),

    -- * World
    World,
    System',
    initWorld,
    destroyEntity,

    -- * Game context
    Game (..),
  )
where

import Apecs
import Linear (V2)
import Wyrdshaper.Spell (VM)
import Wyrdshaper.Tilemap (Tilemap)

-- | An entity's pixel position: the center of its AABB, origin bottom-left,
-- y up (the world convention; the flip to SDL screen space happens in
-- "Wyrdshaper.Engine").
newtype Position = Position (V2 Int)

instance Component Position where type Storage Position = Map Position

-- | Current mana and the regen clock (ticks since the last point back).
-- Player-only.
data Mana = Mana !Int !Int

instance Component Mana where type Storage Mana = Unique Mana

-- | Last nonzero movement direction (components in -1..1); aims 'TileAhead'
-- and slot-1 bolts. Player-only.
newtype Facing = Facing (V2 Int)

instance Component Facing where type Storage Facing = Unique Facing

-- | Present on the player while channeling a spell (which roots movement);
-- absent when idle. Player-only.
newtype Casting = Casting CastState

instance Component Casting where type Storage Casting = Unique Casting

data CastState = CastState
  { castVM :: VM,
    -- | Ticks until the next VM instruction.
    castCooldown :: !Int,
    -- | Instructions executed so far (HUD numerator).
    castSpent :: !Int,
    -- | 'Wyrdshaper.Spell.spellSize' of the program (HUD denominator).
    castSize :: !Int
  }

-- | A bolt in flight: velocity (px\/tick) and remaining ticks to live.
data Projectile = Projectile !(V2 Int) !Int

instance Component Projectile where type Storage Projectile = Map Projectile

-- | A target dummy's remaining hit points.
newtype DummyHP = DummyHP Int

instance Component DummyHP where type Storage DummyHP = Map DummyHP

-- | A fire overlay: the tile alight and ticks of burn left. Burn entities
-- carry no 'Position'; the tile coordinate is the position.
data Burning = Burning !(V2 Int) !Int

instance Component Burning where type Storage Burning = Map Burning

makeWorld "World" [''Position, ''Mana, ''Facing, ''Casting, ''Projectile, ''DummyHP, ''Burning]

type System' a = SystemT World IO a

-- | Every component in the world. 'destroy' only removes the components
-- named in its Proxy, so full deletion must name them all; keep this in
-- sync with the 'makeWorld' list above.
type AllComponents = (Position, Mana, Facing, Casting, Projectile, DummyHP, Burning)

-- | Fully delete an entity. The only way game code should despawn anything.
destroyEntity :: Entity -> System' ()
destroyEntity e = destroy e (Proxy @AllComponents)

-- | Everything the systems need each tick beyond the world itself.
data Game = Game
  { gameMap :: Tilemap,
    gamePlayer :: Entity
  }
