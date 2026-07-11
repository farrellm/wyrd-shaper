# WyrdShaper

Top-down action-adventure where magic is a programming language. Design,
mechanics, and the milestone build plan live in `CONCEPT.md` — read it before
starting a milestone. Current status: M0 (skeleton), M1 (tilemap, player
movement + collision, camera follow), M2 (spell VM, mana, quick slots 1/2/3,
target dummies, HUD bars), M3 (in-game glyph editor on `E`, spellbook
save/load to `spellbook.wyrd` in cwd, Willpower budget), M4 (enemies —
chasers and channeling hexers — player health, faction-tagged bolts,
stagger/fizzle/backlash for every caster, GameOver mode with `R` restart),
and M5 (seeded procgen: noise-biome overworld with the authored start area
stamped in, room-graph dungeon behind stairs, torch-puzzle door keyed to a
loop over the new `UNLIT TORCH` selector, `WYRD_SEED` env var) done.

The game needs the untracked `assets/` directory (gitignored, not
redistributable — see `CREDITS.md`): `Engine.withEngine` loads the UI fonts
from `assets/ui_pack/Fonts/`, and `Terrain.loadTerrain` loads the terrain
sheets from the Franuka packs' **2x (32x32)** variants (`asset_pack/2x/`,
`desert_pack/2x (32x32)/`, `castles_pack/2x (32x32)/`,
`dungeons_fire_pack/2x (32x32)/` — the 2x directory naming varies per
pack), the enemies' monster sheets from
`asset_pack/2x/Monsters and animals/` (Skeleton, Cultist, Mushy — 4x4
grids of 32x32 cells), plus the player's Sorcerer sheets from
`heroes_pack/2x/Character sprites/Sorcerer/` (the heroes pack's 2x cell is
**96x96**, not 32 — the body art inside is about a tile, the rest margin);
startup fails without any of them. 2x art is 1:1 with the 32px tile
grid — nothing on the tile layer is scaled.

## Commands

- Build: `cabal build`
- Run: `cabal run wyrdshaper` (needs a display; opens an SDL window)
- Headless smoke test (no keyboard/mouse injection tools on this machine):
  start `Xvfb :99`, run with `DISPLAY=:99 WYRD_DEMO=1`, screenshot with
  `DISPLAY=:99 import -window root shot.png`. The demo driver in
  `Wyrdshaper.run` auto-casts the quick slots on a frame schedule, runs a
  keyboard-style editor pass (opens slot 2 ~7.5 s, field-edits the volley
  count, commits, casts ~11 s), then a **mouse pass** on slot 3 (~12–17 s):
  synthetic `Input`s (built with `Engine.demoInput`, coordinates derived
  from the live `Editor.buildLayout`) are queued one per frame through the
  real `frame` path — palette drag with snap line (~13.2 s screenshot),
  dropdown pick, row move into a hole, drag-to-palette delete with red tint
  (~16.2 s), commit, cast (~18 s). Each edit logs the buffer's `show` to
  stderr (`grep 'demo editor'`) so runs are assertable without pixels.
  Then an **M4 combat pass** (~18–42 s): walk segments are held-key
  `Engine.demoKeysInput` frames (they reach the ticks through `injRef` — the
  loop's tick closure otherwise sees only real input); when a walk's keys
  release, snap-on-stop glides the player to the next tile center along the
  last motion, so every scripted stop is tile-aligned (walk lengths only
  need to reach anywhere short of the intended tile). The player steps to
  the north door, channels kindle loops timed against the incoming chaser's
  45-tick contact cycle so the hits stagger them (red backlash wash
  ~21.5 s), kills the chaser and the channeling hexer, marches east, dies
  to the far chaser (YOU DIED overlay ~32 s), and restarts via an injected
  `R` tap. The game itself logs every
  beat to stderr (`grep 'combat:'`): player/enemy `cast fizzled (…),
  backlash N`, `player hit`, `enemy slain`, `enemy cast started`,
  `GAME OVER`, `restarted`; runs are frame-for-frame reproducible under
  Xvfb. An **M5 procgen pass** follows (~43–95 s): the demo always runs on
  the fixed `demoSeed` (normal runs take `WYRD_SEED`, else a clock seed —
  `grep 'worldgen: seed'`); the editor pass writes
  `REPEAT 4 { KINDLE UNLIT TORCH }` into slot 3 (`grep 'm5 torch spell'`),
  the player marches out the stamp's south gap and down the carved road
  onto the entrance stairs, bounces once through both stair transitions,
  winds the room tree to the antechamber, lights all four torches with the
  one looped cast, over-loops once to eat the `NoTarget` backlash, and
  walks through the opened door to the shrine. Assert via
  `grep 'dungeon:'`: `entered the dungeon`, `returned to the overworld`,
  `torch lit (1/4)`…`(4/4)`, `all torches burning - the door grinds open`,
  `dungeon complete`; the over-loop shows up as `combat: player cast
  fizzled (NoTarget UnlitTorch), backlash 1`, and `demo pos [...]` lines
  log the tile after each scripted stop. The walk legs come from a BFS
  over the demoSeed maps compressed into straight runs (mask the stairs
  tiles first — they're portals); retune them last if any worldgen
  constant changes. Landing tiles after a stair transition can vary by
  frame/tick drift, so the script normalizes by walking into a wall
  before tuning anything from the position. A **block-view pass** closes
  the script (~95–97 s): the demo's `openDemoEditor` always forces the
  classic `ViewRows` (that's what keeps every scripted pixel coordinate
  and the whole earlier stderr byte-identical), so this pass opens slot 3,
  flips to blocks with a real injected `V` tap, reruns the palette-REPEAT
  drag against the block geometry (same helpers — they read the live
  dispatched `Layout`), logs `demo editor [block drag]`, and Esc-cancels
  so the spellbook is untouched. Give headless runs ~115 s
  before the timeout. Afterwards
  `spellbook.wyrd` holds the edited slots; delete it to restore defaults.

Toolchain: GHC 9.12.4, cabal 3.16, `GHC2024`, `-Wall` (keep the build
warning-free).

## Architecture

- `src/Wyrdshaper/Engine.hs` — the *only* module that imports SDL (sdl2-ttf's
  `SDL.Font` and sdl2-image's `SDL.Image` included): window and renderer
  lifecycle, UI font loading, the per-frame `Input` snapshot, immediate-mode
  rect drawing (`fillWorldRect`, `drawHudBar`, `fillUiRect`), sprite blits
  (opaque `Texture`, `loadTexture`, `drawWorldSprite` — source rect in
  texture pixels, dest in world space, optional color-mod tint), and text
  (`drawText`, `measureText` — textures are created and destroyed per string
  per frame; cache here if a live HUD ever needs lots of text). Rendering is
  stateless — `draw` repaints everything from game state each frame; visuals
  have no entity lifecycle to manage.
- `src/Wyrdshaper/World.hs` — the apecs world: component types and stores,
  the `makeWorld` splice, and `destroyEntity`.
- `src/Wyrdshaper/Loop.hs` — fixed-timestep loop (60 ticks/s). All gameplay
  (movement, the spell VM's per-tick instruction budget) advances in `tick`,
  never per-frame. Input is polled once per frame; every tick of that frame
  sees the same snapshot. UI/mode edge input (editor keys) is handled in the
  once-per-frame `frame` handler, **never** in `tick`: a frame can run zero
  or two ticks, so per-tick edge handling drops or double-fires taps.
- `src/Wyrdshaper/Tilemap.hs` — pure tile world: map parsing, solidity, AABB
  collision (`moveAndCollide`, per-axis pixel sweep — the *single* solidity
  path; the M5 door is a `Tile`, not an entity, for exactly this reason;
  `moveAndCollideCentered` is the same sweep plus a per-step refusal that
  stops the center on the last open tile's center before a solid tile —
  the player's variant, so walking into a wall parks on the grid instead
  of flush).
  M5 widened `Tile` (biome floors `Grass`/`Scrub`/`Swamp`/`Stone`, solid
  `Tree`/`Rock`, `DoorLocked`/`DoorOpen`, `StairsDown`/`StairsUp`,
  `Shrine`) and added `buildTilemap`/`tileAt`/`setTile` for generated and
  mutable maps. Pure module — test collision changes in
  `cabal repl lib:wyrdshaper`, no window needed.
- `src/Wyrdshaper/Worldgen.hs` — pure seeded generation, zero new deps:
  splitmix64-finalizer coordinate hashing, value-noise fbm biomes, the
  160x160 overworld (authored map stamped at `owStampOrigin` with a south
  gap; road to the `StairsDown` clearing carved *always*, so connectivity
  is by construction; biome enemy scatter excluded 8+ tiles from stamp,
  road, and clearing — past hexer aggro, which is what keeps the M2–M4
  demo byte-identical), and the 3x3 room-graph dungeon (randomized-DFS
  spanning tree, 1-wide corridors; the goal room is the tree-farthest
  leaf, so its lone corridor is a cut edge — `DoorLocked` sits where it
  crosses the cell edge, the room before it holds the four torches).
  Must not import `World` (`World` imports it). Verify with
  `overworldOK`/`dungeonOK` — compiled, not interpreted: the 200-seed
  sweep is minutes in ghci, seconds via
  `cabal exec ghc -- -O1 -isrc <Check>.hs`.
- `src/Wyrdshaper/Spell.hs` — pure Wyrdtongue core: the spell AST, the
  small-step VM (`step` runs one instruction; the ECS side charges 1 mana
  per call and paces calls every `castPace` ticks — `ticksPerInstr` for the
  player, `enemyTicksPerInstr` for hexers), and the gameplay tunables block
  (`willpowerMax`, all the M4 combat numbers, `torchLitTicks` — the window
  that makes the M5 door want a loop — and `backlashDamage`: what a
  collapsing cast costs its caster, scaling with mana committed). Takes a
  `WorldView` snapshot in (`wvFoes` plus, since M5, `wvTorches`: the unlit
  torch tiles the `UnlitTorch` selector picks nearest from — rebuilt per
  instruction, so each loop iteration re-selects), emits `Effect`s out —
  repl-testable like Tilemap.
- `src/Wyrdshaper/Terrain.hs` — terrain art: loads the 2x pack sheets once
  (`loadTerrain`) and maps every `Tile` to its draw list (`tileSprites`:
  floor base first, then a transparent feature sprite over it). Variant
  picks are `mix64` on the tile coordinate — stateless, stable across
  frames. Two-case wall autotile (face cell when the tile below is
  walkable, top/fill cell otherwise) for both the castle stamp walls and
  the dungeon. Tree tiles index a 3x3 repeat block of the forest sheet by
  `(1 + x mod 3, 1 + y mod 3)`, so adjacent trees merge into seamless
  canopy. Oversized 2x2-art decor (door arch, shrine circle) is a separate
  `tileDecor` pass so it can overlap neighbors after their bases painted;
  `torchSprite` maps a torch's burn-down counter to the off sprite or one
  of four burning frames (the counter doubles as the animation clock).
  The heroes pack lives here too: `sorcererSprite` picks the player's
  96x96 cell (rows are facings down/left/right/up, columns frames; death >
  cast > walk > idle) off the `Anim` component's free-running tick clock,
  and `sorcererShadow` is the drop-shadow blob under it. The enemies'
  asset_pack monster sheets (`skeletonSprite` for Chasers, `cultistSprite`
  for Hexers, `mushySprite` for Dummies) are 4x4 grids of 32x32 cells with
  the same facing rows; each takes a per-entity salt (the entity id) that
  `mix64`-picks the sheet's color variant, keeping Terrain free of any
  `World` import. Their idle/walk sheets are clean grids — the packs'
  attack/hit/die sheets have irregular geometry with baked-in effect
  overlays, so don't use those.
  Beware Franuka sheet cells that look like ground but are transparent
  decals (e.g. `Stone tile.png`, `Sand_variations.png` rows 0-1 cols 2-3):
  check cell alpha before using one as a base.
- `src/Wyrdshaper/Glyph.hs` — pure editor document model: the glyph subset
  (`ENode`; every block node has exactly one child list, so a cursor `Path`
  is `[Int]`), `flatten` to cursor rows, `insertAt`/`deleteAt`/`modifyAt`,
  `insertionPoints` (drag/snap targets), `moveNode` (subtree move with
  post-delete index adjustment; refuses moves into the moved subtree),
  `fieldOptions` (single source of truth for dropdown menus *and* keyboard
  `-`/`=` cycling — numeric fields wrap, not clamp), display text
  (`rowPieces`), and `compile`/`decompile` to/from the Spell AST.
  Repl-testable; this is where editor logic changes should be tested.
- `src/Wyrdshaper/Spellbook.hs` — quick-slot spellbook: the default spells
  and save/load. Format: a header line + one derived-`Show`n `Stmt` per
  slot; loading falls back per slot (parse failure or over-Willpower) to
  that slot's default with a stderr warning.
- `src/Wyrdshaper/Editor.hs` — the in-game glyph editor, keyboard + mouse
  (Scratch-style drag/snap, dropdown field menus, drag-to-palette delete),
  with two presentations of the same buffer: `ViewBlocks` (the default —
  colored Scratch-like blocks, containers wrapping their inset children in
  a C-shape) and `ViewRows` (the classic flat indented text rows). `V`
  toggles views, preserving buffer/cursor/slot. Both views emit the same
  `Layout`, so `updateEditor` never knows which is showing; only
  `buildLayout`/`drawEditor` dispatch on `edView`. Block geometry is the
  pure `blockGeom` (text widths in, boxes out — repl-test with a fake
  width like `(*8) . length`); its recursion mirrors
  `flatten`/`insertionPoints` exactly, so flat indices, paths, and snap
  points mean the same thing in both views. Block shapes are composed
  `fillUiRect`s — Engine grew no new primitives. All geometry is measured
  once per frame by `buildLayout` and consumed by *both* `updateEditor`
  (hit-testing) and `drawEditor` (pixels) — never compute a rect in one
  and not the other. `updateEditor` is pure given the `Layout` and
  `Input`; handler precedence is the Esc story: open menu > live drag >
  keyboard chain (so Esc closes/cancels innermost first). Drag state
  machine: press → 4 px threshold → drag → drop/cancel; a release lost
  off-window cancels. Drops resolve via the pure `dropTarget` (nearest gap
  by y, ties by indent x), shared with the snap-indicator drawing.
- `src/Wyrdshaper.hs` — game setup (`spawnLevel`, shared with restart: the
  player's components are re-`set` on the same entity id so `gamePlayer`
  stays valid), tick systems, `draw`, and the `Shell` (mode + spellbook in
  an `IORef` owned by the loop closures — apecs `Global` stores can't join
  `AllComponents`, and it's meta-state, not simulation state). Since M5
  the map is level state: `Game` holds the immutable per-run `Overworld`
  and `Dungeon` plus an `IORef Level` (place, mutable tilemap, puzzle
  latches); systems read it via `curMap`. `tickLevel` runs *last* in
  `tick` — the torch-count door unlock (a `setTile` on the level map), the
  shrine check, and the stair transitions all live there, so no system
  ever sees a half-swapped level; `enterDungeon`/`exitDungeon` reuse the
  `restartGame` teardown shape but spare the player entity (HP/mana carry
  across; re-entry regenerates from the cached gen results, re-locking the
  door). Torches are `Position`+`Torch` entities; `KindleEff` lights a
  torch instead of ground fire when one owns the tile, and `tickTorches`
  stops burning them down once the door is open. `draw` culls tiles to the
  camera rect (a 160x160 map would otherwise dwarf every other draw). Movement is
  free (3 px/tick) while keys are held, except that the player never moves
  past the middle of the last open tile before a wall
  (`moveAndCollideCentered` — enemies and shoves keep flush
  `moveAndCollide`); on release `tickInput` glides the player to the next
  tile center along the last motion (`snapTarget` — never backwards), so an
  idle player always sits on the grid — walked into a wall too, with no
  snap-back — and `TileAhead` spells read predictably. Combat: one
  `tickCast` `cmapM_` advances every caster (player and hexers) under the
  same rules; every cast collapse — stagger, `NoTarget`, `OutOfMana`, … —
  routes through `fizzleCast`, which destroys `Casting` *first* (no
  re-stagger) and applies `backlashDamage` via raw `hurt` (bypassing
  i-frames: the stagger hit just granted some). `damageEntity` is the gated
  path (player `Invuln`, stagger check reads `Maybe Casting` *before*
  `hurt` — the hit may destroy the entity); bolts and `foeSnapshot` are
  faction-filtered. While the editor is open or in `GameOver` the
  simulation is frozen (no ticks; `frame` flips a dead player to `GameOver`
  and handles the `R` restart). Escape quits when playing or dead; in the
  editor it cancels (`runLoop` checks quit before `frame`, so one Escape
  can't do both). `app/Main.hs` is a stub.

## Conventions

- World coordinates are **pixels, origin bottom-left, y up**. Tilemap row 0
  is the bottom row; map source strings are top-first and reversed at parse.
  Tiles are 32 px. SDL screen space is top-left origin, y down — the flip
  happens only inside `Engine.fillWorldRect`; world rendering never thinks
  in screen coordinates. UI overlays (`drawHudBar`, `fillUiRect`,
  `drawText`, i.e. the HUD and the editor panel) are the exception: they
  take screen-space positions directly.
- Entity positions are AABB centers (`Position`); collision boxes span the
  half-open interval [center − half, center + half).
- Draw order is explicit and back-to-front in `Wyrdshaper.draw`: tile bases
  (`tileSprites`), oversized tile decor (`tileDecor`, cull widened a tile
  for overhang, far rows first), player (drop shadow, then the Sorcerer
  sprite anchored feet-to-body-bottom at `p + V2 0 4`), enemies (with
  in-world HP/cast bars), torch sprites, bolts, fire, HUD, damage wash; the
  editor panel or game-over veil (when applicable) draws on top of it all,
  and `presentFrame` is called by the loop's draw closure, not by `draw`.
  Enemies are asset_pack monster sprites (Skeleton/Cultist/Mushy) over the
  player's shadow blob, anchored with the same `+ V2 0 4` trick; every
  sprite's hurt flash is a red tint (texture color-mod only darkens —
  white can't flash a sprite) and the player's i-frames blink via alpha.

## apecs / sdl2 notes (apecs 0.10, sdl2 2.5.6)

- `destroy` removes only the components named in its `Proxy` — deleting an
  entity means naming the full tuple. Always use `World.destroyEntity`, and
  when adding a component to `makeWorld`, add it to `AllComponents` too
  (nested tuples: apecs instances stop at 8 elements). A leak there
  surfaces as ghost state after a `GameOver` restart.
- Every component uses a `Map` store (M4 moved `Mana`/`Facing`/`Casting`
  off `Unique` so enemy casters share the player's machinery); state that
  comes and goes is component presence (`Casting` while channeling,
  `Invuln`/`HitFlash` timers), read with `get e :: … (Maybe c)` — component
  removal works fine in apecs, and a non-`Maybe` `get` of an absent `Map`
  component is a *runtime* crash the types won't catch.
- In `cmap`/`cmapM_`/`cfold` tuples, put `Entity` last: member enumeration
  comes from the first component's store. Destroying the current entity
  inside `cmapM_` is safe (members are snapshotted first).
- sdl2 config is StateVar-style (`rendererDrawColor r $= …`). Vsync is
  chosen at renderer creation (`AcceleratedVSyncRenderer`).
- `pollEvents` must run before `getKeyboardState` or the held-key snapshot
  is stale; edge detection (`inTapped`) must filter `keyboardEventRepeat`
  or held keys re-fire on OS key repeat.
