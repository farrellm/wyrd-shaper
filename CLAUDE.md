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

Currently on **M0 — Skeleton** (window + a quad moving under an aztecs
system, per `CONCEPT.md`). Next up: **M1 — Player & world**.

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
