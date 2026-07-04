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
    isWallAtPx,
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

parseMap :: [String] -> (TileMap, V2 Int)
parseMap rows =
  ( TileMap [[if c == '#' then Wall else Floor | c <- row] | row <- rows],
    case starts of
      (p : _) -> p
      [] -> error "Tilemap: rawMap has no '@' start marker"
  )
  where
    starts =
      [ tileCenterPx (col, rowTop)
        | (rowTop, row) <- zip [0 ..] rows,
          (col, c) <- zip [0 ..] row,
          c == '@'
      ]

tileMap :: TileMap
playerStartPx :: V2 Int
(tileMap, playerStartPx) = parseMap rawMap

-- | Spawn the tilemap's renderable entities parented to the given world
-- entity: one big background quad for the floor, plus one quad per wall
-- tile. Floor tiles don't get individual quads -- 'Rectangle' spawns its
-- own private mesh/material entity per call with no automatic batching
-- across entities, so one background quad is far cheaper for no visual
-- difference.
spawnTiles :: EntityID -> Access IO ()
spawnTiles worldEntity = do
  spawn_ $
    bundle (Rectangle (fromIntegral mapWidthPx) (fromIntegral mapHeightPx))
      <> bundle
        ( transform2d {transformTranslation = V2 (mapWidthPx `div` 2) (mapHeightPx `div` 2)} ::
            Transform2D
        )
      <> bundle (color 0.15 0.15 0.18 1)
      <> bundle (Parent worldEntity)
  mapM_ spawnWall wallPositions
  where
    wallPositions =
      [ (col, rowTop)
        | (rowTop, row) <- zip [0 ..] (tileRows tileMap),
          (col, t) <- zip [0 ..] row,
          t == Wall
      ]
    spawnWall pos =
      spawn_ $
        bundle (Rectangle (fromIntegral tileSize) (fromIntegral tileSize))
          <> bundle (transform2d {transformTranslation = tileCenterPx pos} :: Transform2D)
          <> bundle (color 0.45 0.40 0.35 1)
          <> bundle (Parent worldEntity)

rawMap :: [String]
rawMap =
  [ "########################################",
    "#......................................#",
    "#.@....................................#",
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
