# WyrdShaper

Design and build plan: `CONCEPT.md`. Read it for the game design, the
Wyrtongue language, and the full milestone list — this file only covers
engineering setup and things that aren't obvious from the code.

## Tech stack

- Haskell, GHC2024, cabal (GHC 9.12.4, cabal-install 3.16.1.0 via ghcup).
- Engine: [`aztecs`](https://github.com/aztecs-hs/aztecs) — a young
  (pre-1.0), archetype-based ECS. Companion packages currently in use:
  `aztecs-gl` (OpenGL rendering) and `aztecs-glfw` (windowing/input).

## Build & run

```
cabal build
cabal run wyrdshaper
```

### Setup prerequisite (Linux)

`aztecs-glfw` links against the system GLFW library. On Arch/Manjaro:

```
sudo pacman -S glfw
```

Without it, the build succeeds (dependency resolution doesn't check system
libs) but linking fails. There's no `DISPLAY`/`WAYLAND_DISPLAY` check at
build time either — running the executable in a session without a display
server will hit `GLFW.createWindow` returning `Nothing`, which the
`aztecs-glfw` `Window` component turns into an `error "TODO"` crash. This
is an upstream limitation, not a bug in this repo.

## Milestone status

M0 (Skeleton) and M1 (Player & world: tilemap, keyboard movement with tile
collision, camera follow) are done, per `CONCEPT.md`. Next up: **M2 — Spell
VM core**.

## aztecs gotchas (verified against source, current as of aztecs-0.17.1 / aztecs-gl-0.3.0 / aztecs-glfw-0.2.0)

These aren't obvious from reading the code and cost real research time to
pin down — worth keeping until covered by better upstream docs.

- **`aztecs-hierarchy` is a stale, separate Hackage package** (pinned to
  `aztecs >=0.12 && <0.13` — incompatible with current `aztecs`). Don't
  depend on it. `Aztecs.Hierarchy` (which provides `Parent`) now lives
  inside the core `aztecs` package and is re-exported by the top-level
  `Aztecs` module.
- **`aztecs-sdl` is archived.** GLFW + `aztecs-gl` (OpenGL) is the actively
  maintained rendering path, not SDL.
- **`render` (`Aztecs.GL.D2`) only draws entities parented to a window.**
  Any renderable entity needs a `Parent windowEntity` component or it's
  silently skipped.
- **`runAccessGLFW` does not throttle the loop at all** — no vsync,
  no swap-interval, no delay. It just polls events, runs one tick, swaps
  buffers, and recurses as fast as possible. A stable tick rate has to be
  implemented in application code (e.g. `threadDelay`).
- **`Component` instances need `{-# LANGUAGE TypeFamilies #-}`** in the
  defining module, even for trivial instances with no explicit `StorageT`
  override — the class has an associated type family.
- GHC2024 already includes `TypeApplications`, `FlexibleInstances`,
  `MultiParamTypeClasses`, and `NumericUnderscores`, so example code
  targeting Haskell2010 (upstream examples repo) carries pragmas for these
  that are redundant here.
- **`Rectangle w h` is centered on its `Transform2D` translation** (mesh
  vertices run `-w/2..w/2`, `-h/2..h/2`), not corner-anchored. Matters for
  any tile/grid layout math.
- **No camera/viewport component exists** anywhere in `aztecs-gl` or
  `aztecs-transform`. `render` projects with a fixed orthographic matrix
  over the `Window`'s raw pixel dimensions. To fake camera scrolling: parent
  all world-space entities to one "world" entity and update *its* local
  `Transform2D` each tick — every entity's `Transform2D` automatically gets
  a `GlobalTransform2D` (`localT <> parentGlobalT`) that recursively
  propagates to `Children` whenever a parent's local transform changes
  (`aztecs-transform`'s `Component` instance), and `render` always draws
  from `GlobalTransform2D`, never the local one. So moving the one parent
  entity moves everything under it for free.
- **`Keys` can only be read via `lookup @_ @Keys windowEntity`** in the
  top-level `Access` monad closure passed to `runAccessGLFW` — there's no
  way to read it from inside a `Query`-based `system`. Fetch it once per
  tick and thread it manually into plain `lookup`/`insert` calls on specific
  entities (mirrors the upstream `aztecs-examples` `Pong.hs` pattern).
  `keyPressed` is the continuously-held check (for movement); `keyJustPressed`/
  `keyJustUnpressed` are edge-triggered.
- **No collision/AABB helpers exist** anywhere in `aztecs`, `aztecs-gl`, or
  `aztecs-transform` — hand-roll tile/AABB checks (see `app/Tilemap.hs`).
