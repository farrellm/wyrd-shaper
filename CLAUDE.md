# WyrdShaper

Top-down action-adventure where magic is a programming language. Design,
mechanics, and the milestone build plan live in `CONCEPT.md` — read it before
starting a milestone. Current status: M0 (skeleton) done.

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
- `src/Wyrdshaper.hs` — game setup and systems; `app/Main.hs` is a stub.

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
