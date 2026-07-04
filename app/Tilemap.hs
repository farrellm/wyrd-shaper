module Tilemap
  ( Tile (..),
    TileMap,
    tileSize,
    mapWidthTiles,
    mapHeightTiles,
    mapWidthPx,
    mapHeightPx,
    tileMap,
    playerStartPx,
    boulderStartTile,
    torchTile,
    isWallAtPx,
    isWallAtTile,
    pixelToTile,
    tileCenterPx,
    tileFacingFromMovement,
    spawnTiles,
  )
where

import Aztecs
import Aztecs.GL.D2
import Prelude hiding (lookup)

tileSize :: Int
tileSize = 32

data Tile = Floor | Wall
  deriving (Eq, Show)

-- | Tile rows exactly as authored: row 0 is the top row on screen.
newtype TileMap = TileMap {tileRows :: [[Tile]]}

mapWidthTiles, mapHeightTiles :: Int
mapWidthTiles = case rawMap of
  (row : _) -> length row
  [] -> error "Tilemap: rawMap is empty"
mapHeightTiles = length rawMap

mapWidthPx, mapHeightPx :: Int
mapWidthPx = mapWidthTiles * tileSize
mapHeightPx = mapHeightTiles * tileSize

-- | Center of tile (col, rowFromTop) in world pixel space. Rendering is
-- Y-up with origin bottom-left, but the map is authored top-to-bottom, so
-- this is the one place that flip happens -- collision, parsing, and
-- spawning all go through this (or its inverse, 'pixelToTile') and never
-- do row arithmetic themselves.
tileCenterPx :: (Int, Int) -> V2 Int
tileCenterPx (col, rowTop) =
  V2
    (col * tileSize + tileSize `div` 2)
    (mapHeightPx - (rowTop * tileSize + tileSize `div` 2))

-- | Inverse of 'tileCenterPx': a world pixel coordinate to its containing
-- (col, rowFromTop) tile.
pixelToTile :: V2 Int -> (Int, Int)
pixelToTile (V2 x y) = (x `div` tileSize, mapHeightTiles - 1 - (y `div` tileSize))

tileAt :: TileMap -> (Int, Int) -> Tile
tileAt (TileMap rows) (col, rowTop)
  | rowTop < 0 || rowTop >= mapHeightTiles = Wall
  | col < 0 || col >= mapWidthTiles = Wall
  | otherwise = (rows !! rowTop) !! col

isWallAtPx :: TileMap -> V2 Int -> Bool
isWallAtPx tm p = tileAt tm (pixelToTile p) == Wall

-- | Tile-coordinate counterpart of 'isWallAtPx', for callers (spell effects)
-- that already work in (col, rowTop) space and shouldn't round-trip through
-- pixels just to ask "is this tile a wall".
isWallAtTile :: TileMap -> (Int, Int) -> Bool
isWallAtTile tm pos = tileAt tm pos == Wall

-- | Find the (col, rowTop) tile coordinate of the first occurrence of a
-- marker character in the authored map, top row first. Errors if the marker
-- doesn't appear -- markers passed here are all required set pieces (player
-- start, boulder, torch), same contract as the old inline '@' search this
-- replaces.
findMarkerTile :: Char -> [String] -> (Int, Int)
findMarkerTile marker rows =
  case positions of
    (p : _) -> p
    [] -> error $ "Tilemap: rawMap has no '" ++ [marker] ++ "' marker"
  where
    positions =
      [ (col, rowTop)
        | (rowTop, row) <- zip [0 ..] rows,
          (col, c) <- zip [0 ..] row,
          c == marker
      ]

-- | Convert a raw movement delta (pixel/world space, Y-up) into a unit
-- (colDelta, rowTopDelta) step in tile space, applying the same flip as
-- 'tileCenterPx'/'pixelToTile' so callers never do row arithmetic
-- themselves. Diagonal input resolves to vertical priority (arbitrary but
-- explicit tie-break -- there's no meaningful diagonal "tile ahead").
-- 'Nothing' if there's no movement to derive a facing from.
tileFacingFromMovement :: V2 Int -> Maybe (Int, Int)
tileFacingFromMovement (V2 dx dy)
  | dy /= 0 = Just (0, negate (signum dy))
  | dx /= 0 = Just (signum dx, 0)
  | otherwise = Nothing

parseMap :: [String] -> (TileMap, V2 Int)
parseMap rows =
  ( TileMap [[if c == '#' then Wall else Floor | c <- row] | row <- rows],
    tileCenterPx (findMarkerTile '@' rows)
  )

tileMap :: TileMap
playerStartPx :: V2 Int
(tileMap, playerStartPx) = parseMap rawMap

-- | Boulder and torch start tiles, found the same way as the player start.
boulderStartTile, torchTile :: (Int, Int)
boulderStartTile = findMarkerTile 'B' rawMap
torchTile = findMarkerTile 'K' rawMap

-- | Spawn the tilemap's renderable entities: one big background quad for
-- the floor, plus one quad per wall tile. Floor tiles don't get individual
-- quads -- 'Rectangle' spawns its own private mesh/material entity per call
-- with no automatic batching across entities, so one background quad is far
-- cheaper for no visual difference.
--
-- Each entity is spawned with 'Parent windowEntity' (not 'worldEntity')
-- because 'Rectangle's mesh registration only fires if the entity's
-- /immediate/ parent carries the window's raw handle -- it doesn't walk
-- further up the hierarchy. Once that one-time registration has happened,
-- the entity is reparented to 'worldEntity' so it participates in the
-- camera-follow transform hierarchy instead.
spawnTiles :: EntityID -> EntityID -> Access IO ()
spawnTiles windowEntity worldEntity = do
  floorE <-
    spawn $
      bundle (Rectangle (fromIntegral mapWidthPx) (fromIntegral mapHeightPx))
        <> bundle
          ( transform2d {transformTranslation = V2 (mapWidthPx `div` 2) (mapHeightPx `div` 2)} ::
              Transform2D
          )
        <> bundle (color 0.15 0.15 0.18 1)
        <> bundle (Parent windowEntity)
  insert floorE $ bundle (Parent worldEntity)
  mapM_ spawnWall wallPositions
  where
    wallPositions =
      [ (col, rowTop)
        | (rowTop, row) <- zip [0 ..] (tileRows tileMap),
          (col, t) <- zip [0 ..] row,
          t == Wall
      ]
    spawnWall pos = do
      wallE <-
        spawn $
          bundle (Rectangle (fromIntegral tileSize) (fromIntegral tileSize))
            <> bundle (transform2d {transformTranslation = tileCenterPx pos} :: Transform2D)
            <> bundle (color 0.45 0.40 0.35 1)
            <> bundle (Parent windowEntity)
      insert wallE $ bundle (Parent worldEntity)

rawMap :: [String]
rawMap =
  [ "########################################",
    "#......................................#",
    "#.@..B..K..............................#",
    "#......................................#",
    "#.......######........########.........#",
    "#.......######........########.........#",
    "#.......######........########.........#",
    "#.......######........########.........#",
    "#......................................#",
    "#......................................#",
    "#......................................#",
    "#......................................#",
    "#..............##########..............#",
    "#..............##########..............#",
    "#......................................#",
    "#......................................#",
    "#.....####....................####.....#",
    "#.....####....................####.....#",
    "#.....####....................####.....#",
    "#.....####....................####.....#",
    "#.....####....................####.....#",
    "#.....####....................####.....#",
    "#......................................#",
    "#......................................#",
    "#.............############.............#",
    "#.............############.............#",
    "#.............############.............#",
    "#......................................#",
    "#......................................#",
    "########################################"
  ]
