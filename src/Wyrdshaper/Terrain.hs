-- | Terrain art: which sprite(s) each map tile draws.
--
-- Loads the 2x (32x32) Franuka pack sheets once at startup and maps every
-- 'Tile' to a draw list — a floor base first, then a feature sprite with
-- transparency over it. Tiles are 32 world px, so every blit is 1:1 with
-- the art; nothing on the tile layer is scaled. Variant picks (which rock,
-- whether a grass tile grows flowers) come from 'mix64' on the tile
-- coordinate: stateless and stable across frames, like all worldgen
-- randomness. Oversized 2x2-art decorations (the dungeon door and the
-- shrine circle) are a separate pass ('tileDecor') so they can overlap
-- neighboring tiles *after* those tiles' bases have painted.
module Wyrdshaper.Terrain
  ( Terrain,
    loadTerrain,
    SpriteDraw (..),
    tileSprites,
    tileDecor,
    torchSprite,
  )
where

import Data.Word (Word64)
import Linear (V2 (..))
import Wyrdshaper.Engine (Color (..), Gfx, Texture, loadTexture)
import Wyrdshaper.Tilemap
import Wyrdshaper.Worldgen (mix64)

-- | One blit: sheet, source rect position and size in texture pixels, and
-- an optional tint multiplied onto the texture's color.
data SpriteDraw = SpriteDraw Texture (V2 Int) (V2 Int) (Maybe Color)

-- | The loaded terrain sheets. Field names follow the source packs.
data Terrain = Terrain
  { -- asset_pack (overworld)
    txGrass :: Texture,
    txCoast :: Texture,
    txRoad :: Texture,
    txForest :: Texture,
    txRocks :: [Texture],
    txCaveEntrance :: Texture,
    txGrassDecor :: [Texture],
    -- desert_pack (scrub)
    txSand :: Texture,
    -- castles_pack (the authored stamp's walls)
    txCastleWall :: Texture,
    -- dungeons_fire_pack
    txDgFloorA :: Texture,
    txDgFloorB :: Texture,
    txDgWalls :: Texture,
    txDgStairsDown :: Texture,
    txDgStairsUp :: Texture,
    txDoorClosed :: Texture,
    txDoorOpen :: Texture,
    txCircle :: Texture,
    txTorchOff :: Texture,
    txTorchOn :: Texture
  }

-- | Load every sheet. Fails fast with a pointer at the gitignored
-- @assets/@ when a file is missing, like the UI fonts.
loadTerrain :: Gfx -> IO Terrain
loadTerrain gfx = do
  let ap p = "assets/asset_pack/2x/Tileset/" ++ p
      dp p = "assets/desert_pack/2x (32x32)/Tileset/" ++ p
      cp p = "assets/castles_pack/2x (32x32)/Tiles/" ++ p
      fp p = "assets/dungeons_fire_pack/2x (32x32)/" ++ p
      load = loadTexture gfx
  txGrass' <- load (ap "Grass.png")
  txCoast' <- load (ap "Coastlines.png")
  txRoad' <- load (ap "Roads_stone.png")
  txForest' <- load (ap "Forest tiles_1.png")
  txRocks' <- mapM (load . ap) ["Rock_1.png", "Rock_2.png", "Rock_3.png", "Rock_4.png"]
  txCaveEntrance' <- load (ap "Cave entrance_1.png")
  txGrassDecor' <-
    mapM
      (load . ap)
      ["Tall grass.png", "Flower_1.png", "Flower_5.png", "Flower_9.png", "Leaves_1.png"]
  txSand' <- load (dp "Sand_variations.png")
  txCastleWall' <- load (cp "Interior Walls 1.png")
  txDgFloorA' <- load (fp "Tiles/Floors1.png")
  txDgFloorB' <- load (fp "Tiles/Floors2.png")
  txDgWalls' <- load (fp "Tiles/Walls2.png")
  txDgStairsDown' <- load (fp "Tiles/Stairs1.png")
  txDgStairsUp' <- load (fp "Tiles/Stairs2.png")
  txDoorClosed' <- load (fp "Objects & Decoration/Door_Closed.png")
  txDoorOpen' <- load (fp "Objects & Decoration/Door_Open.png")
  txCircle' <- load (fp "Objects & Decoration/SummoningCircle_Off.png")
  txTorchOff' <- load (fp "Objects & Decoration/Torch_Floor_Off.png")
  txTorchOn' <- load (fp "Objects & Decoration/Torch_Floor_On.png")
  pure
    Terrain
      { txGrass = txGrass',
        txCoast = txCoast',
        txRoad = txRoad',
        txForest = txForest',
        txRocks = txRocks',
        txCaveEntrance = txCaveEntrance',
        txGrassDecor = txGrassDecor',
        txSand = txSand',
        txCastleWall = txCastleWall',
        txDgFloorA = txDgFloorA',
        txDgFloorB = txDgFloorB',
        txDgWalls = txDgWalls',
        txDgStairsDown = txDgStairsDown',
        txDgStairsUp = txDgStairsUp',
        txDoorClosed = txDoorClosed',
        txDoorOpen = txDoorOpen',
        txCircle = txCircle',
        txTorchOff = txTorchOff',
        txTorchOn = txTorchOn'
      }

-- | A whole single-texture sprite (the one-tile PNGs).
whole :: Texture -> Maybe Color -> SpriteDraw
whole tex = SpriteDraw tex (V2 0 0) (V2 tileSize tileSize)

-- | A 32x32 cell of a sheet, addressed (column, row) from the top-left.
cell :: Texture -> V2 Int -> Maybe Color -> SpriteDraw
cell tex cxy = SpriteDraw tex ((* tileSize) <$> cxy) (V2 tileSize tileSize)

-- | Stateless per-tile randomness; @salt@ separates decisions made about
-- the same tile.
tileHash :: Word64 -> V2 Int -> Word64
tileHash salt (V2 x y) =
  mix64 (mix64 (salt + fromIntegral x) + fromIntegral y)

-- | The swamp is tinted grass: there is no swamp ground in the packs.
swampTint :: Maybe Color
swampTint = Just (Color 0.45 0.8 0.95 1)

-- | The sprites for one tile, drawn in order (base first). @inDungeon@
-- picks the skin for the tiles both levels share (Floor, Wall, stairs).
tileSprites :: Terrain -> Bool -> Tilemap -> V2 Int -> Tile -> [SpriteDraw]
tileSprites tr inDungeon tm xy t = case t of
  -- overworld biome floors
  Grass -> grassBase Nothing
  Swamp -> grassBase swampTint
  Scrub ->
    let h = tileHash 0xa11ce xy
     in [ cell (txSand tr) (sandCell h) Nothing
        ]
  -- cracked stone pavement: the carved road and the stony biome
  Stone -> [cell (txRoad tr) (V2 1 1) Nothing]
  -- the authored stamp (grass courtyard) and the dungeon interior
  Floor
    | inDungeon -> [dungeonFloor tr xy]
    | otherwise -> grassBase Nothing
  Wall
    | inDungeon ->
        [ if faceRow
            then cell (txDgWalls tr) (V2 1 4) Nothing -- brick face
            else cell (txDgWalls tr) (V2 7 1) Nothing -- dark top
        ]
    | otherwise ->
        [ if faceRow
            then cell (txCastleWall tr) (V2 1 1) Nothing -- bricks + base trim
            else cell (txCastleWall tr) (V2 1 0) Nothing -- plain bricks
        ]
  Water -> [cell (txCoast tr) (V2 4 1) Nothing]
  -- solid overworld features on a borrowed floor base
  Tree ->
    let V2 x y = xy
     in floorBase ++ [cell (txForest tr) (V2 (1 + x `mod` 3) (1 + y `mod` 3)) Nothing]
  Rock ->
    let h = tileHash 0x70c4 xy
     in floorBase ++ [whole (txRocks tr !! fromIntegral (h `mod` 4)) Nothing]
  -- transitions and goals
  StairsDown
    | inDungeon -> dungeonFloor tr xy : [whole (txDgStairsDown tr) Nothing]
    | otherwise -> floorBase ++ [whole (txCaveEntrance tr) Nothing]
  StairsUp -> dungeonFloor tr xy : [whole (txDgStairsUp tr) Nothing]
  -- the door and shrine draw their oversized art in 'tileDecor'; here
  -- they are just floor
  DoorLocked -> [dungeonFloor tr xy]
  DoorOpen -> [dungeonFloor tr xy]
  Shrine -> [dungeonFloor tr xy]
  where
    -- Grass, sometimes wearing a flower/tall-grass/leaves decal (the base
    -- sheet's plain cell is flat green; the decals are the texture).
    grassBase tint =
      let h = tileHash 0x92a55 xy
          decor = txGrassDecor tr
       in cell (txGrass tr) (V2 1 1) tint
            : [ whole (decor !! fromIntegral (h `div` 8 `mod` fromIntegral (length decor))) tint
                | h `mod` 8 == 0
              ]
    -- Sand: mostly the four crack variants, an occasional darker patch.
    sandCell h
      | h `mod` 16 == 0 = V2 1 1
      | otherwise = V2 (fromIntegral (h `mod` 4)) 2
    -- A wall shows its face when it can be seen past: the tile below is
    -- not another wall.
    faceRow =
      let V2 x y = xy
       in tileAt tm (V2 x (y - 1)) /= Just Wall
    -- What a solid feature sits on: the floor of the first walkable
    -- 4-neighbor, defaulting to grass.
    floorBase =
      let V2 x y = xy
          neighbors = [V2 x (y - 1), V2 (x - 1) y, V2 (x + 1) y, V2 x (y + 1)]
          floors =
            [ nt
              | n <- neighbors,
                Just nt <- [tileAt tm n],
                nt `elem` [Grass, Swamp, Scrub, Stone, Floor]
            ]
       in case floors of
            nt : _ | s : _ <- tileSprites tr inDungeon tm xy nt -> [s]
            _ -> [cell (txGrass tr) (V2 1 1) Nothing]

-- | The hashed A\/B dungeon floor.
dungeonFloor :: Terrain -> V2 Int -> SpriteDraw
dungeonFloor tr xy =
  let h = tileHash 0xf100 xy
   in whole (if h `mod` 4 == 0 then txDgFloorB tr else txDgFloorA tr) Nothing

-- | Oversized decorations (2x2-tile art on a 1-tile anchor), drawn after
-- every visible tile's 'tileSprites' so they may overlap neighbors:
-- sprite, world extents, and world offset from the anchor tile's center.
tileDecor :: Terrain -> Bool -> V2 Int -> Tile -> [(SpriteDraw, V2 Int, V2 Int)]
tileDecor tr inDungeon _xy t = case t of
  -- The arch stands on the door tile and rises over the wall above it.
  DoorLocked | inDungeon -> [arch (txDoorClosed tr)]
  DoorOpen | inDungeon -> [arch (txDoorOpen tr)]
  Shrine -> [(SpriteDraw (txCircle tr) (V2 0 0) (V2 64 64) Nothing, V2 64 64, V2 0 0)]
  _ -> []
  where
    arch tex =
      (SpriteDraw tex (V2 0 0) (V2 64 64) Nothing, V2 64 64, V2 0 (tileSize `div` 2))

-- | The torch entity's sprite: a cold floor torch, or one of the four
-- burning frames — the door-counting burn-down timer doubles as the
-- animation clock.
torchSprite :: Terrain -> Int -> SpriteDraw
torchSprite tr lit
  | lit <= 0 = whole (txTorchOff tr) Nothing
  | otherwise = cell (txTorchOn tr) (V2 ((lit `div` 8) `mod` 4) 0) Nothing
