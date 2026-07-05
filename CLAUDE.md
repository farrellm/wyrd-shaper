# WyrdShaper

Top-down action-adventure where magic is a programming language. Design,
mechanics, and the milestone build plan live in `CONCEPT.md` — read it before
starting a milestone. Current status: M0 (skeleton) and M1 (tilemap, player
movement + collision, camera follow) done.

## Commands

- Build: `cabal build`
- Run: `cabal run wyrdshaper` (needs a display; opens a GLFW window)

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
- `src/Wyrdshaper.hs` — game setup and systems; `app/Main.hs` is a stub.

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
- Upstream `render` hardcodes the origin; camera follow is
  `Wyrdshaper.Engine.renderWithCamera` (a patched copy — one modelview
  translate). Entities render from their auto-maintained `GlobalTransform2D`.
- aztecs-gl 0.3.0 ships a leftover `Debug.Trace` in register/unregister —
  one `[RenderGroups] ...` stderr line per individually registered entity is
  expected noise, not an error.
