# WyrdShaper

Top-down action-adventure where magic is a programming language. Design,
mechanics, and the milestone build plan live in `CONCEPT.md` — read it before
starting a milestone. Current status: M0 (skeleton), M1 (tilemap, player
movement + collision, camera follow), and M2 (spell VM, mana, quick slots
1/2/3, target dummies, HUD bars) done.

## Commands

- Build: `cabal build`
- Run: `cabal run wyrdshaper` (needs a display; opens an SDL window)
- Headless smoke test (no keyboard injection tools on this machine): start
  `Xvfb :99`, run with `DISPLAY=:99 WYRD_DEMO=1` — the demo driver in
  `Wyrdshaper.run` auto-casts the quick slots on a tick schedule — and
  screenshot with `DISPLAY=:99 import -window root shot.png`.

Toolchain: GHC 9.12.4, cabal 3.16, `GHC2024`, `-Wall` (keep the build
warning-free).

## Architecture

- `src/Wyrdshaper/Engine.hs` — the *only* module that imports SDL: window
  and renderer lifecycle, the per-frame `Input` snapshot, and immediate-mode
  rect drawing (`fillWorldRect`, `drawHudBar`). Rendering is stateless —
  `draw` repaints everything from game state each frame; visuals have no
  entity lifecycle to manage.
- `src/Wyrdshaper/World.hs` — the apecs world: component types and stores,
  the `makeWorld` splice, and `destroyEntity`.
- `src/Wyrdshaper/Loop.hs` — fixed-timestep loop (60 ticks/s). All gameplay
  (movement, the spell VM's per-tick instruction budget) advances in `tick`,
  never per-frame. Input is polled once per frame; every tick of that frame
  sees the same snapshot.
- `src/Wyrdshaper/Tilemap.hs` — pure tile world: map parsing, solidity, AABB
  collision (`moveAndCollide`, per-axis pixel sweep). Pure module — test
  collision changes in `cabal repl lib:wyrdshaper`, no window needed.
- `src/Wyrdshaper/Spell.hs` — pure Wyrdtongue core: the spell AST, the
  small-step VM (`step` runs one instruction; the ECS side charges 1 mana
  per call and paces calls every `ticksPerInstr` ticks), and the gameplay
  tunables block. Takes a `WorldView` snapshot in, emits `Effect`s out —
  repl-testable like Tilemap.
- `src/Wyrdshaper.hs` — game setup, tick systems, and `draw`; `app/Main.hs`
  is a stub.

## Conventions

- World coordinates are **pixels, origin bottom-left, y up**. Tilemap row 0
  is the bottom row; map source strings are top-first and reversed at parse.
  Tiles are 32 px. SDL screen space is top-left origin, y down — the flip
  happens only inside `Engine.fillWorldRect`/`drawHudBar`; nothing else may
  think in screen coordinates.
- Entity positions are AABB centers (`Position`); collision boxes span the
  half-open interval [center − half, center + half).
- Draw order is explicit and back-to-front in `Wyrdshaper.draw`: tiles,
  player, dummies, bolts, fire, HUD.

## apecs / sdl2 notes (apecs 0.10, sdl2 2.5.6)

- `destroy` removes only the components named in its `Proxy` — deleting an
  entity means naming the full tuple. Always use `World.destroyEntity`, and
  when adding a component to `makeWorld`, add it to `AllComponents` too.
- Player-only components (`Mana`, `Facing`, `Casting`) use `Unique` stores;
  state that comes and goes is component presence (`Casting` while
  channeling), read with `get e :: … (Maybe c)` — component removal works
  fine in apecs.
- In `cmap`/`cmapM_`/`cfold` tuples, put `Entity` last: member enumeration
  comes from the first component's store. Destroying the current entity
  inside `cmapM_` is safe (members are snapshotted first).
- sdl2 config is StateVar-style (`rendererDrawColor r $= …`). Vsync is
  chosen at renderer creation (`AcceleratedVSyncRenderer`).
- `pollEvents` must run before `getKeyboardState` or the held-key snapshot
  is stale; edge detection (`inTapped`) must filter `keyboardEventRepeat`
  or held keys re-fire on OS key repeat.
