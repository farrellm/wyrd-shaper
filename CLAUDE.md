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

M0 (Skeleton), M1 (Player & world: tilemap, keyboard movement with tile
collision, camera follow), and M2 (Spell VM core: `Verb`/`Selector`/`Instr`
AST in `app/Spell.hs`, a ticked coroutine interpreter with per-instruction
windup in `app/Main.hs`'s `tick`, mana, and `bolt`/`push`/`kindle` castable
from quick slots `1`/`2`/`3`) are done, per `CONCEPT.md`. Next up: **M3 —
Block editor**.

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
- **Shape components (`Rectangle`/`Circle`/`Triangle`) only register as
  renderable if their *immediate* `Parent` is the window entity at the
  moment the shape component is inserted** — `inParentWindowContext`
  (`Aztecs.GL.Internal`) checks exactly one hop, it doesn't walk further up
  the ancestor chain. Get this wrong (e.g. parent straight to an
  intermediate "world"/camera entity instead) and the shape's mesh is
  never compiled and never added to `RenderGroups` — no error, just a
  black window. If you need the entity to *also* live under a different
  parent for hierarchy/transform purposes (see the camera gotcha below),
  spawn it with `Parent windowEntity` first, then immediately `insert` a
  new `Parent` pointing at the real parent — `Parent`'s `componentOnChange`
  (`Aztecs.Hierarchy`) correctly moves it between the old and new parent's
  `Children` sets, and `GlobalTransform2D` propagation (see below) follows
  the *current* `Children`, not whatever was current at spawn time. This
  cost real research time to track down (a M1/M2 regression that made the
  window render solid black) — see `spawnTiles` in `app/Tilemap.hs` and the
  player/boulder/torch/`spawnImpact` spawns in `app/Main.hs` for the pattern.
- **`runAccessGLFW` does not throttle the loop at all** — no vsync,
  no swap-interval, no delay. It just polls events, runs one tick, swaps
  buffers, and recurses as fast as possible. A stable tick rate has to be
  implemented in application code (e.g. `threadDelay`).
- **`Component` instances need an associated-type-family extension in
  scope** (the class has `type StorageT a`), but **GHC2024 already includes
  `TypeFamilies`** — verified by actually removing the
  `{-# LANGUAGE TypeFamilies #-}` pragma from `app/Main.hs` (which defines
  this repo's first custom `Component` instances) and confirming it still
  compiles. Kept the explicit pragma anyway for clarity/robustness against a
  different `default-language`, but it's redundant under GHC2024 specifically
  — don't be alarmed if upstream example code (Haskell2010, explicit pragma
  lists) needs it and this repo doesn't.
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
  entity moves everything under it for free. Renderables must still be
  spawned `Parent windowEntity` first and reparented to the world entity
  afterward — see the shape-registration gotcha above.
- **`Keys` can only be read via `lookup @_ @Keys windowEntity`** in the
  top-level `Access` monad closure passed to `runAccessGLFW` — there's no
  way to read it from inside a `Query`-based `system`. Fetch it once per
  tick and thread it manually into plain `lookup`/`insert` calls on specific
  entities (mirrors the upstream `aztecs-examples` `Pong.hs` pattern).
  `keyPressed` is the continuously-held check (for movement); `keyJustPressed`/
  `keyJustUnpressed` are edge-triggered.
- **No collision/AABB helpers exist** anywhere in `aztecs`, `aztecs-gl`, or
  `aztecs-transform` — hand-roll tile/AABB checks (see `app/Tilemap.hs`).
- **`despawn` does not run component lifecycle hooks** — verified from
  source: `Aztecs.ECS.Access.despawn` calls `World.despawn` →
  `Entities.despawn`, and unlike `insert`/`remove`/`spawn` there's no
  `unAccess hook` anywhere in that path. Calling it directly on a
  renderable, `Parent`-ed entity skips `Rectangle`/`Material`'s hooks (VBO +
  render-group cleanup) and, worse, `Parent`'s hook — the thing that detaches
  the entity from its parent's `Children` set. Left unremoved, `Children`
  only grows forever and transform propagation (which walks the full
  `Children` set on every parent-transform change) gets silently slower every
  tick for the rest of the session — no crash, just an unbounded leak. Always
  `remove` hook-owning components (`Rectangle`, `Material`, `Parent`, ...)
  before `despawn`-ing anything renderable/parented — see
  `despawnRenderable` in `app/Main.hs`.
- **`Aztecs.ECS.Query`/`system` (not just direct `EntityID` `lookup`/
  `insert`) is the idiomatic way to iterate an unknown-size, dynamically
  spawned set of entities** — e.g. `app/Main.hs`'s `stepLifetimes`, which
  decrements every live `Lifetime` component and despawns expired ones via
  `system . runQuery $ (,) <$> entity <*> queryMapAccum step`. This mirrors
  `aztecs-examples/src/SpriteSheet.hs`'s `animate`. Reach for this when the
  entity count isn't known in advance (M1/M2's fixed entities — player,
  world, one boulder, one torch — were hand-tracked via a `GameEntities`
  record instead, which is simpler when the set is small and static).
