-- | Seeded procedural generation: the noise-biome overworld and the
-- room-graph dungeon. Everything here is pure and deterministic in the
-- seed — repl-testable like Tilemap, and the source of truth the demo's
-- walk segments are tuned against.
--
-- Randomness is a stateless coordinate hash (the splitmix64 finalizer),
-- not a sequence generator: value noise wants @hash(seed, x, y)@, which
-- keeps the module zero-dependency. Sequential choices (the dungeon
-- layout) use a tiny counter-based 'Rng' over the same mixer.
--
-- This module must not import "Wyrdshaper.World" ("Wyrdshaper.World"
-- imports it), so enemy placements are 'SpawnKind's that the ECS side
-- maps to its own enemy kinds.
module Wyrdshaper.Worldgen
  ( Seed,
    mix64,
    Biome (..),
    biomeAt,
    SpawnKind (..),
    Overworld (..),
    generateOverworld,
    Dungeon (..),
    generateDungeon,
    reachable,
    overworldOK,
    dungeonOK,
  )
where

import Data.Bits (shiftR, xor)
import Data.List (maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Word (Word64)
import Linear (V2 (..))
import Wyrdshaper.Tilemap

type Seed = Word64

-- * Hashing and noise

-- | The splitmix64 finalizer: a high-quality 64-bit mixer.
mix64 :: Word64 -> Word64
mix64 z0 =
  let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in z2 `xor` (z2 `shiftR` 31)

-- | Stateless coordinate hash.
hash2 :: Seed -> V2 Int -> Word64
hash2 seed (V2 x y) =
  mix64 (seed + fromIntegral x * 0x9E3779B97F4A7C15 + fromIntegral y * 0xD1B54A32D192ED03)

-- | Uniform in [0, 1).
rand01 :: Seed -> V2 Int -> Double
rand01 seed xy = fromIntegral (hash2 seed xy `shiftR` 11) / 9007199254740992

-- | Lattice value noise: bilinear interpolation (smoothstepped) of corner
-- hashes on a grid of the given period.
valueNoise :: Seed -> Int -> V2 Int -> Double
valueNoise seed period (V2 x y) =
  let (cx, fx) = x `divMod` period
      (cy, fy) = y `divMod` period
      u = smooth (fromIntegral fx / fromIntegral period)
      v = smooth (fromIntegral fy / fromIntegral period)
      corner dx dy = rand01 seed (V2 (cx + dx) (cy + dy))
      lerp t a b = a + t * (b - a)
      smooth t = t * t * (3 - 2 * t)
   in lerp v (lerp u (corner 0 0) (corner 1 0)) (lerp u (corner 0 1) (corner 1 1))

-- | Weighted sum of value-noise octaves, normalized to [0, 1).
fbm :: Seed -> [(Int, Double)] -> V2 Int -> Double
fbm seed octaves xy =
  sum [w * valueNoise (mix64 (seed + fromIntegral k)) p xy | (k, (p, w)) <- zip [1 :: Int ..] octaves]
    / sum (map snd octaves)

-- | Counter-based sequence RNG for the dungeon's ordered choices.
newtype Rng = Rng Word64

next64 :: Rng -> (Word64, Rng)
next64 (Rng s) = let s' = s + 0x9E3779B97F4A7C15 in (mix64 s', Rng s')

-- | Uniform in [lo, hi] (inclusive).
nextR :: (Int, Int) -> Rng -> (Int, Rng)
nextR (lo, hi) rng =
  let (w, rng') = next64 rng
   in (lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1)), rng')

shuffle :: [a] -> Rng -> ([a], Rng)
shuffle xs rng0 = go xs rng0
  where
    go [] rng = ([], rng)
    go ys rng =
      let (i, rng') = nextR (0, length ys - 1) rng
          (a, b) = splitAt i ys
          (rest, rng'') = go (a ++ drop 1 b) rng'
       in (ys !! i : rest, rng'')

-- * Biomes

data Biome = Forest | Marsh | Scrubland | Mountain
  deriving (Eq, Show)

-- | Two fbm channels — elevation picks mountains, moisture splits the
-- lowlands into marsh, forest, and scrubland.
biomeAt :: Seed -> V2 Int -> Biome
biomeAt seed xy
  | elevation > 0.62 = Mountain
  | moisture > 0.62 = Marsh
  | moisture > 0.38 = Forest
  | otherwise = Scrubland
  where
    elevation = fbm (mix64 (seed + 11)) [(48, 0.6), (24, 0.3), (12, 0.1)] xy
    moisture = fbm (mix64 (seed + 22)) [(32, 0.7), (16, 0.3)] xy

-- * The overworld

data SpawnKind = SpawnChaser | SpawnHexer
  deriving (Eq, Show)

data Overworld = Overworld
  { -- | 160x160 tiles.
    owMap :: Tilemap,
    -- | Player start in world pixels (the authored map's @\@@, stamped).
    owStart :: V2 Int,
    -- | Tile offset of the authored 48x37 stamp.
    owStampOrigin :: V2 Int,
    -- | The 'StairsDown' tile.
    owEntrance :: V2 Int,
    -- | Biome-biased enemy placements (tile coordinates).
    owSpawns :: [(SpawnKind, V2 Int)]
  }

owSize :: Int
owSize = 160

stampOrigin :: V2 Int
stampOrigin = V2 56 62

-- | The west column of the 2-tile gap carved in the stamp's south wall
-- (authored-relative x; the M2-M4 demo never goes near the south wall,
-- so the stamp stays inert).
stampGapX :: Int
stampGapX = 23

-- | A seed makes a world: rock rim, the authored map stamped whole at
-- 'stampOrigin' with a south gap, noise biomes with hashed feature
-- scatter, and an always-carved road from the gap to a dungeon-entrance
-- clearing — connectivity by construction, for every seed.
generateOverworld :: Seed -> Overworld
generateOverworld seed =
  Overworld
    { owMap = setTiles (roadTiles ++ clearingTiles) base,
      owStart = fmap (* tileSize) stampOrigin + authoredStart,
      owStampOrigin = stampOrigin,
      owEntrance = entrance,
      owSpawns = spawns
    }
  where
    (authored, authoredStart) = worldMap
    stampW = mapWidth authored
    stampH = mapHeight authored

    -- Entrance: south of the gap, snapped so the road is two straight legs.
    rng0 = Rng (mix64 (seed + 77))
    (dist, rng1) = nextR (25, 45) rng0
    (ex, _) = nextR (30, 130) rng1
    V2 gapX gapY = stampOrigin + V2 stampGapX 0
    ey = gapY - dist
    entrance = V2 ex ey

    base = buildTilemap owSize owSize synth
    synth xy@(V2 x y)
      | Just rel <- inStamp xy =
          if relGap rel then Grass else fromMaybe Wall (tileAt authored rel)
      | x < 2 || y < 2 || x >= owSize - 2 || y >= owSize - 2 = Rock
      | otherwise = biomeTile xy
    inStamp (V2 x y) =
      let rel@(V2 rx ry) = V2 x y - stampOrigin
       in if rx >= 0 && ry >= 0 && rx < stampW && ry < stampH then Just rel else Nothing
    relGap (V2 rx ry) = ry == 0 && (rx == stampGapX || rx == stampGapX + 1)
    biomeTile xy = case biomeAt seed xy of
      Forest -> if rand01 featSeed xy < 0.14 then Tree else Grass
      Marsh -> if valueNoise poolSeed 6 xy > 0.7 then Water else Swamp
      Mountain -> if rand01 featSeed xy < 0.25 then Rock else Stone
      Scrubland -> if rand01 featSeed xy < 0.04 then Rock else Scrub
    featSeed = mix64 (seed + 33)
    poolSeed = mix64 (seed + 44)

    -- The road: a south leg from the gap, then an east/west leg to the
    -- clearing, both ending on the stairs at the clearing's center.
    roadTiles =
      [(V2 x y, Grass) | x <- [gapX, gapX + 1], y <- [ey .. gapY - 1]]
        ++ [(V2 x y, Grass) | y <- [ey, ey + 1], x <- [min gapX ex .. max (gapX + 1) ex]]
    clearingTiles =
      [(entrance + V2 dx dy, Stone) | dx <- [-2 .. 2], dy <- [-2 .. 2]]
        ++ [(entrance, StairsDown)]

    -- Enemy scatter, biome-biased, kept clear of the stamp, the road, and
    -- the clearing — all inflated past hexer aggro (7 tiles), so the M2-M4
    -- demo timings can't shift and the road is a safe walk.
    spawnSeed = mix64 (seed + 55)
    kindSeed = mix64 (seed + 66)
    spawns =
      take 40 $
        [ (kind, xy)
        | y <- [2 .. owSize - 3],
          x <- [2 .. owSize - 3],
          let xy = V2 x y,
          not (excluded xy),
          not (solid (synth xy)),
          Just kind <- [pick xy]
        ]
    pick xy = case biomeAt seed xy of
      Forest | roll < 1 / 300 -> Just SpawnChaser
      Marsh | roll < 1 / 300 -> Just SpawnHexer
      Scrubland
        | roll < 1 / 300 ->
            Just (if even (hash2 kindSeed xy) then SpawnChaser else SpawnHexer)
      Mountain | roll < 1 / 1200 -> Just SpawnChaser
      _ -> Nothing
      where
        roll = rand01 spawnSeed xy
    excluded (V2 x y) =
      inRect (stampOrigin - 8) (stampOrigin + V2 stampW stampH + 8)
        || inRect (V2 (gapX - 8) (ey - 8)) (V2 (gapX + 9) (gapY + 8))
        || inRect (V2 (min gapX ex - 8) (ey - 8)) (V2 (max (gapX + 1) ex + 8) (ey + 9))
        || inRect (entrance - 10) (entrance + 10)
      where
        inRect (V2 lx ly) (V2 hx hy) = x >= lx && x <= hx && y >= ly && y <= hy

-- * The dungeon

data Dungeon = Dungeon
  { dgMap :: Tilemap,
    -- | Where the player lands on entry (beside 'dgExit', never on it).
    dgEntry :: V2 Int,
    -- | The 'StairsUp' tile back to the overworld.
    dgExit :: V2 Int,
    -- | The 'DoorLocked' tile guarding the goal room.
    dgDoor :: V2 Int,
    -- | The four torch tiles in the antechamber.
    dgTorches :: [V2 Int],
    -- | The 'Shrine' tile.
    dgGoal :: V2 Int,
    dgSpawns :: [(SpawnKind, V2 Int)]
  }

cellsAcross, cellW, cellH :: Int
cellsAcross = 3
cellW = 17
cellH = 13

-- | Inclusive tile bounds of a room.
data Room = Room {roomLo :: V2 Int, roomHi :: V2 Int}

roomCenter :: Room -> V2 Int
roomCenter (Room lo hi) = fmap (`div` 2) (lo + hi)

roomTiles :: Room -> [V2 Int]
roomTiles r =
  let V2 lx ly = roomLo r
      V2 hx hy = roomHi r
   in [V2 x y | y <- [ly .. hy], x <- [lx .. hx]]

-- | A seed makes a dungeon: a 3x3 cell grid of rooms joined by a
-- randomized-DFS spanning tree with 1-wide corridors. The goal room is
-- the tree-farthest leaf, so its single corridor is a guaranteed cut
-- edge — 'DoorLocked' sits where that corridor crosses into the goal
-- cell, and the room before it is the torch antechamber.
generateDungeon :: Seed -> Dungeon
generateDungeon seed =
  Dungeon
    { dgMap = dmap,
      dgEntry = stairs + V2 1 0,
      dgExit = stairs,
      dgDoor = door,
      dgTorches = torches,
      dgGoal = goal,
      dgSpawns = sideSpawns
    }
  where
    w = cellsAcross * cellW
    h = cellsAcross * cellH
    cells = [V2 cx cy | cy <- [0 .. cellsAcross - 1], cx <- [0 .. cellsAcross - 1]]
    entryCell = V2 1 0

    -- One room per cell, inset at least one tile from the cell edge.
    -- Minimum sizes keep room centers (corridor junctions) off the
    -- inset-1 corners, where the stairs and torches go.
    (rooms, rngRooms) = foldl' place (M.empty, Rng (mix64 (seed + 5))) cells
    place (acc, rng) cell@(V2 cx cy) =
      let (rw, r1) = nextR (6, 13) rng
          (rh, r2) = nextR (5, 9) r1
          (ox, r3) = nextR (1, cellW - 1 - rw) r2
          (oy, r4) = nextR (1, cellH - 1 - rh) r3
          lo = V2 (cx * cellW + ox) (cy * cellH + oy)
       in (M.insert cell (Room lo (lo + V2 (rw - 1) (rh - 1))) acc, r4)
    roomOf cell = rooms M.! cell

    -- Randomized DFS spanning tree over the cells.
    treeEdges = dfs [entryCell] (S.singleton entryCell) rngRooms []
    dfs [] _ _ acc = acc
    dfs (c : stack) seen rng acc =
      let nbrs = [n | d <- [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)], let n = c + d, inGrid n, not (S.member n seen)]
          (order, rng') = shuffle nbrs rng
       in case order of
            [] -> dfs stack seen rng' acc
            (n : _) -> dfs (n : c : stack) (S.insert n seen) rng' ((c, n) : acc)
    inGrid (V2 cx cy) = cx >= 0 && cy >= 0 && cx < cellsAcross && cy < cellsAcross

    parent = M.fromList [(c, p) | (p, c) <- treeEdges]
    depth :: V2 Int -> Int
    depth c = maybe 0 (\p -> 1 + depth p) (M.lookup c parent)
    goalCell = maximumBy (comparing depth) [c | c <- cells, c /= entryCell]
    anteCell = parent M.! goalCell
    pathCells = entryCell : go goalCell
      where
        go c = c : maybe [] go (M.lookup c parent)

    -- L-shaped corridor between the room centers of adjacent cells:
    -- across at the source room's row/column, then along the target's.
    -- Every tile stays inside the two cells, so a corridor can only ever
    -- touch its own two rooms.
    corridor a b =
      let V2 ax ay = roomCenter (roomOf a)
          V2 bx by = roomCenter (roomOf b)
       in if ay == by || abs (ax - bx) > abs (ay - by) -- horizontal neighbors
            then
              [V2 x ay | x <- [min ax bx .. max ax bx]]
                ++ [V2 bx y | y <- [min ay by .. max ay by]]
            else
              [V2 ax y | y <- [min ay by .. max ay by]]
                ++ [V2 x by | x <- [min ax bx .. max ax bx]]

    -- The goal corridor's boundary tile inside the goal cell: the one
    -- corridor into the goal cell crosses the cell edge exactly once, so
    -- blocking that tile cuts the goal room off.
    door =
      let V2 gx gy = goalCell
          onEdge (V2 x y) = case goalCell - anteCell of
            V2 1 _ -> x == gx * cellW
            V2 (-1) _ -> x == gx * cellW + cellW - 1
            V2 _ 1 -> y == gy * cellH
            _ -> y == gy * cellH + cellH - 1
       in case filter onEdge (corridor anteCell goalCell) of
            t : _ -> t
            [] -> error "worldgen: goal corridor never crosses the cell edge"

    carved =
      S.fromList $
        concatMap (roomTiles . roomOf) cells
          ++ concatMap (uncurry corridor) treeEdges
    dmap =
      setTiles
        ([(door, DoorLocked), (goal, Shrine), (stairs, StairsUp)])
        (buildTilemap w h (\xy -> if S.member xy carved then Floor else Wall))

    goal = roomCenter (roomOf goalCell)
    stairs = roomLo (roomOf entryCell) + V2 1 1
    torches =
      let Room (V2 lx ly) (V2 hx hy) = roomOf anteCell
       in [V2 (lx + 1) (ly + 1), V2 (hx - 1) (ly + 1), V2 (lx + 1) (hy - 1), V2 (hx - 1) (hy - 1)]

    sideSpawns =
      [ (SpawnChaser, roomCenter (roomOf c))
      | c <- take 2 [c | c <- cells, c `notElem` pathCells]
      ]

-- * Generation properties (repl checks; the demo relies on these)

-- | Breadth-first search over non-solid tiles, 4-connected.
reachable :: Tilemap -> V2 Int -> V2 Int -> Bool
reachable tm from to = go (S.singleton from) [from]
  where
    go _ [] = False
    go seen (c : rest)
      | c == to = True
      | otherwise =
          let nbrs =
                [ n
                | d <- [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)],
                  let n = c + d,
                  not (S.member n seen),
                  maybe False (not . solid) (tileAt tm n)
                ]
           in go (foldl' (flip S.insert) seen nbrs) (rest ++ nbrs)

-- | The start reaches the entrance stairs, and every spawn stands on
-- walkable ground.
overworldOK :: Overworld -> Bool
overworldOK ow =
  tileAt (owMap ow) (owEntrance ow) == Just StairsDown
    && reachable (owMap ow) (fmap (`div` tileSize) (owStart ow)) (owEntrance ow)
    && all (\(_, xy) -> maybe False (not . solid) (tileAt (owMap ow) xy)) (owSpawns ow)

-- | With the door locked the entry reaches every torch and the door's
-- threshold but not the shrine; opening the door makes the shrine
-- reachable. This is the puzzle being both required and sufficient.
dungeonOK :: Dungeon -> Bool
dungeonOK dg =
  all (reachable (dgMap dg) (dgEntry dg)) (dgTorches dg)
    && any
      (\d -> reachable (dgMap dg) (dgEntry dg) (dgDoor dg + d))
      [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
    && not (reachable (dgMap dg) (dgEntry dg) (dgGoal dg))
    && reachable (setTile (dgDoor dg) DoorOpen (dgMap dg)) (dgEntry dg) (dgGoal dg)
