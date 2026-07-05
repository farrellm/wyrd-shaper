-- | Hand-authored tile world: the grid, solidity, and AABB collision.
--
-- World coordinates are pixels with the origin at the map's bottom-left and
-- y increasing upward (matching the renderer's orthographic projection).
-- Row 0 of a 'Tilemap' is the bottom row; map source text is top-first and
-- gets reversed at parse time.
module Wyrdshaper.Tilemap
  ( Tile (..),
    Tilemap,
    tileSize,
    solid,
    parseTilemap,
    worldMap,
    tiles,
    tileCenter,
    mapWidth,
    mapHeight,
    boxHitsSolid,
    moveAndCollide,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear (V2 (..))

data Tile = Floor | Wall | Water
  deriving (Eq, Show)

-- | Rows of tiles, row 0 at the bottom.
newtype Tilemap = Tilemap (Vector (Vector Tile))

-- | Tile edge length in pixels.
tileSize :: Int
tileSize = 32

solid :: Tile -> Bool
solid Floor = False
solid Wall = True
solid Water = True

-- | Parse top-first rows of map text. @#@ wall, @~@ water, @.@ floor,
-- @\@@ floor and player start. Returns the map and the start position in
-- world pixels (center of the @\@@ tile, or map center if absent).
parseTilemap :: [String] -> (Tilemap, V2 Int)
parseTilemap rowsTopFirst =
  let rows = reverse rowsTopFirst
      w = maximum (0 : map length rows)
      pad r = take w (r ++ repeat '#')
      tile c = case c of
        '#' -> Wall
        '~' -> Water
        _ -> Floor
      tm = Tilemap . V.fromList $ map (V.fromList . map tile . pad) rows
      starts =
        [ tileCenter (V2 tx ty)
        | (ty, r) <- zip [0 ..] rows,
          (tx, c) <- zip [0 ..] r,
          c == '@'
        ]
      fallback = V2 (w * tileSize `div` 2) (length rows * tileSize `div` 2)
   in (tm, case starts of s : _ -> s; [] -> fallback)

mapWidth, mapHeight :: Tilemap -> Int
mapWidth (Tilemap rows) = if V.null rows then 0 else V.length (V.head rows)
mapHeight (Tilemap rows) = V.length rows

-- | Every tile with its grid coordinates.
tiles :: Tilemap -> [(V2 Int, Tile)]
tiles (Tilemap rows) =
  [ (V2 tx ty, t)
  | (ty, row) <- zip [0 ..] (V.toList rows),
    (tx, t) <- zip [0 ..] (V.toList row)
  ]

-- | World-pixel center of a tile.
tileCenter :: V2 Int -> V2 Int
tileCenter (V2 tx ty) = V2 (tx * tileSize + tileSize `div` 2) (ty * tileSize + tileSize `div` 2)

-- | Out-of-bounds counts as solid.
solidAtTile :: Tilemap -> Int -> Int -> Bool
solidAtTile tm@(Tilemap rows) tx ty
  | tx < 0 || ty < 0 || tx >= mapWidth tm || ty >= mapHeight tm = True
  | otherwise = solid $ rows V.! ty V.! tx

-- | Does an AABB (center, half-extents) overlap any solid tile? The box
-- occupies the half-open pixel span [center - half, center + half).
boxHitsSolid :: Tilemap -> V2 Int -> V2 Int -> Bool
boxHitsSolid tm (V2 hx hy) (V2 cx cy) =
  or
    [ solidAtTile tm tx ty
    | tx <- [(cx - hx) `div` tileSize .. (cx + hx - 1) `div` tileSize],
      ty <- [(cy - hy) `div` tileSize .. (cy + hy - 1) `div` tileSize]
    ]

-- | Move an AABB by a delta, sliding along solid tiles: the x axis is swept
-- first, then y, one pixel at a time (deltas are a few pixels per tick).
moveAndCollide :: Tilemap -> V2 Int -> V2 Int -> V2 Int -> V2 Int
moveAndCollide tm he p (V2 dx dy) =
  let px = sweep (\n (V2 x y) -> V2 (x + n) y) dx p
   in sweep (\n (V2 x y) -> V2 x (y + n)) dy px
  where
    sweep move d q0
      | d == 0 = q0
      | otherwise = go (abs d) q0
      where
        go 0 q = q
        go n q =
          let q' = move (signum d) q
           in if boxHitsSolid tm he q' then q else go (n - 1) q'

-- | The hand-authored M1 map (48x37 tiles).
worldMap :: (Tilemap, V2 Int)
worldMap =
  parseTilemap
    [ "################################################",
      "#..............................#...............#",
      "#..............................#...............#",
      "#....######....................#....######.....#",
      "#....#....#....................#....#.....#....#",
      "#....#....#....######..........#....#.....#....#",
      "#....##..##....#....#..........##..##.....#....#",
      "#..............#....#.....................#....#",
      "#..............##..##.....................#....#",
      "#..........................######..#######.....#",
      "#...~~~~....................#..................#",
      "#..~~~~~~~..................#..................#",
      "#..~~~~~~~~~................#......~~~~........#",
      "#...~~~~~~~~~...............#....~~~~~~~~......#",
      "#....~~~~~~~................#...~~~~~~~~~~.....#",
      "#......~~~..................#....~~~~~~~~......#",
      "#.................................~~~~.........#",
      "#..............................................#",
      "#.....########..########.......................#",
      "#.....#................#.......................#",
      "#.....#................#........########.......#",
      "#.....#.......@........#........#......#.......#",
      "#.....#................#........#......#.......#",
      "#.....#................#........##....##.......#",
      "#.....########..########.......................#",
      "#..............................................#",
      "#..............................................#",
      "#....#####################.....................#",
      "#....#...................#.........~~~~~~......#",
      "#....#...................#........~~~~~~~~.....#",
      "#....#####...........#####.........~~~~~~......#",
      "#..............................................#",
      "#..............................................#",
      "#...######......######......######......###....#",
      "#..............................................#",
      "#..............................................#",
      "################################################"
    ]
