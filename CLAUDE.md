# WyrdShaper

Top-down action-adventure where magic is a programming language. Design,
mechanics, and the milestone build plan live in `CONCEPT.md` — read it before
starting a milestone. Current status: M0 (skeleton), M1 (tilemap, player
movement + collision, camera follow), and M2 (spell VM, mana, quick slots
1/2/3, target dummies, HUD bars) done.

## Commands

- Build: `cabal build`
- Run: `cabal run wyrdshaper` (needs a display; opens a GLFW window)
- Headless smoke test (no keyboard injection tools on this machine): start
  `Xvfb :99`, run with `DISPLAY=:99 WYRD_DEMO=1` — the demo driver in
  `Wyrdshaper.run` auto-casts the quick slots on a tick schedule — and
  screenshot with `DISPLAY=:99 import -window root shot.png`.

Toolchain: GHC 9.12.4, cabal 3.16, `GHC2024`, `-Wall` (keep the build
warning-free).

## Architecture

- `src/Wyrdshaper/Engine.hs` — the *only* module that imports aztecs
  packages directly; everything else imports `Wyrdshaper.Engine`. The aztecs
  stack is pre-1.0 and churns, so deps are pinned exactly in
  `wyrdshaper.cabal` and engine API exposure stays in this one layer.
- `src/Wyrdshaper/Loop.hs` — fixed-timestep loop (60 ticks/s). All gameplay
  (movement, and later the spell VM's per-tick instruction budget) advances
  in `tick`, never per-frame.
- `src/Wyrdshaper/Tilemap.hs` — pure tile world: map parsing, solidity, AABB
  collision (`moveAndCollide`, per-axis pixel sweep). Pure module — test
  collision changes in `cabal repl lib:wyrdshaper`, no window needed.
- `src/Wyrdshaper/Spell.hs` — pure Wyrdtongue core: the spell AST, the
  small-step VM (`step` runs one instruction; the ECS side charges 1 mana
  per call and paces calls every `ticksPerInstr` ticks), and the gameplay
  tunables block. Takes a `WorldView` snapshot in, emits `Effect`s out —
  repl-testable like Tilemap.
- `src/Wyrdshaper.hs` — game setup, components (`Mana`, `Facing`,
  `CastState`, `Projectile`, `DummyHP`, `Burning`), and tick systems;
  `app/Main.hs` is a stub.

## Conventions

- World coordinates are **pixels, origin bottom-left, y up** (matches the GL
  ortho projection). Tilemap row 0 is the bottom row; map source strings are
  top-first and reversed at parse. Tiles are 32 px.
- Entity positions are AABB centers; collision boxes span the half-open
  interval [center − half, center + half).

## aztecs gotchas (verified against aztecs 0.17.1 / aztecs-gl 0.3.0)

- Renderable entities must get their `Parent windowEntity` component
  **before** shape components (`Rectangle`, etc.): the shape's insert hook
  finds the OpenGL context through the parent. Use
  `Wyrdshaper.Engine.spawnColoredRect`, which encodes the ordering.
- Record update on `transform2d` re-generalizes the translation type
  parameter — annotate the result (`:: Transform2D`) or GHC reports an
  ambiguous `Typeable` instance.
- `runAccessGLFW` takes an `Access m Bool` run once per frame; returning
  `True` quits. It polls events, refreshes the window entity's `Keys`/`Cursor`
  components, and swaps buffers itself.
- Screen coordinates: orthographic 0..width / 0..height with origin at the
  **bottom-left**; `Transform2D` translation is `V2 Int` in pixels.
- Query pattern for read-then-write systems:
  `system . readQuery $ (,) <$> entity <*> query`, then `lookup`/`insert`
  per entity.
- **Never call `registerRenderable` per entity for bulk spawns** (it
  re-inserts the whole `RenderGroups` map and prints a debug trace each
  call — quadratic). Use `Wyrdshaper.Engine.registerInstances` with a shared
  mesh (`sharedRectMesh`) and material (`colorMaterial`); instances then need
  only a `Transform2D` — no `Parent`/`OfMesh`/`OfMaterial`.
- `despawn` runs **no** component-remove hooks, and instances aren't in the
  hooks' reach anyway: call `Wyrdshaper.Engine.unregisterInstances` before
  despawning anything registered via `registerInstances`, or it leaks a
  `RenderGroups` entry.
- **`remove` is broken upstream**: moving an entity to an *existing*
  archetype never adds it to that archetype's entity set
  (`Archetypes.remove` updates storages only), so the entity's remaining
  components misalign and every `lookup` on it fails afterward. Never remove
  a component to express state — keep the component and make the state a
  value (e.g. `Channeling (Maybe CastState)` on the player).
- Spawn entities with their **complete bundle** (`bundle a <> bundle b`),
  not spawn-then-`insert`: an insert that moves an entity into an existing
  archetype only lines storages up correctly when the mover has the highest
  entity id of the group. Full-bundle spawns (ids are monotonic, never
  reused) and in-place value updates are always safe.
- Render groups draw in `RenderGroupKey` (mesh, material entity-id) order —
  create shared meshes/materials in back-to-front draw order at setup.
- Upstream `render` hardcodes the origin; camera follow is
  `Wyrdshaper.Engine.renderWithCamera` (a patched copy — one modelview
  translate). Entities render from their auto-maintained `GlobalTransform2D`.
- aztecs-gl 0.3.0 ships a leftover `Debug.Trace` in register/unregister —
  one `[RenderGroups] ...` stderr line per individually registered entity is
  expected noise, not an error.
