# WyrdShaper

A top-down action-adventure in the spirit of 16-bit Zelda, where magic is a
programming language and knowledge is literally power. You play a *wyrdshaper*
— one who rewrites fate by speaking the world's true language. Spells are not
found in chests or bought from vendors: you write them, as small programs,
and the depth of what you can express grows with your understanding of the
world itself.

## Design Pillars

1. **Magic is programming.** The spell system *is* the skill tree. Player
   power comes from mastering constructs — sequencing, conditionals, loops,
   and eventually recursion and higher-order spells — not from bigger numbers
   on gear.
2. **Knowledge is power.** Capability is gated by understanding. New
   constructs, verbs, and eventually the leap from glyphs to script come
   from engaging with the language and the world's rules — power grows from
   what you have learned to express, not from gear.
3. **The world is legible.** Procedurally generated terrain and dungeons obey
   consistent rules. If fire spreads through dry grass once, it always will —
   the world is a machine the player can learn to program.
4. **Zelda-like moment-to-moment feel.** Real-time, readable, top-down.
   Whatever depth lives in the spell editor, the second-to-second experience
   is moving, dodging, and casting under pressure.

## Core Mechanics

### The Wyrdtongue

The Wyrdtongue is the language the world was spoken in — and spells are
programs written in it.

Every spell, from the crudest firebolt to the endgame's self-modifying wards,
is built from one core language:

- **Values** — numbers, directions, durations.
- **Selectors** — expressions that resolve to targets at runtime: `nearest
  foe`, `self`, `tile ahead`, `everything burning within 5`.
- **Effect verbs** — the primitive actions: `bolt`, `push`, `shield`,
  `kindle`, `douse`, `mend`, `hush`…
- **Control** — sequencing, `if`, bounded loops, `let` bindings.
- **Triggers** — wards that wrap a spell body: *when struck*, *when a foe
  crosses this line*, *when the flame dies*.

#### Stage one: glyphs (block editor)

Early on, spells are assembled from **glyph blocks** in a visual editor —
snap-together pieces in the style of Scratch. The palette is small at first
(a selector, a verb, maybe two verbs in sequence) and grows as you level:
conditionals, then loops, then variables and wards.

Two budgets keep early spells honest:

- **Willpower** caps program length — how many glyphs you can hold in mind at
  once. It grows with level.
- **Mana** is spent per instruction executed, not per cast. A loop that
  fires three bolts costs three bolts.

#### Stage two: script (textual language)

Mid-game, you learn to *write* the Wyrdtongue. The textual language contains
everything the glyph editor can express — any glyph spell can be viewed as
text — but it is a strict **superset**. Blocks cap out at loops and
conditionals. Text unlocks what glyphs cannot say:

- **Named spells with parameters** — define `chain n t` once, invoke it
  anywhere.
- **Recursion** — `chain` can call itself.
- **Spells as values** — pass a spell to a spell: metamagic, delayed casts,
  wards that install other wards.

```
spell chain n t:
  if n > 0:
    bolt of fire at t
    chain (n - 1) (next foe after t)
```

Graduating from glyphs to script is a story beat, not just a menu unlock —
the moment the training wheels come off and the real language opens up.

### Ticked Execution and Backlash

Casting is **channeling**. A spell does not resolve instantly: the
interpreter runs a few instructions per game tick while you stand and speak,
and the world keeps moving around you.

- Loops visibly repeat — a three-bolt loop launches its bolts one after
  another, ticks apart.
- Longer, more complex programs take longer to cast. Program structure is a
  tactical decision, not just an intellectual one.
- **Interruption**: taking a stagger mid-cast fizzles the spell, and the
  **backlash** — wild energy released by the collapsing spell — scales with
  the mana already committed. Big spells are big risks.
- **Runtime errors backlash too.** A selector that finds no target, mana
  exhausted mid-loop, recursion that never bottoms out — the spell collapses
  and bites its caster. Debugging is a survival skill.

Counterplay runs both ways: enemy casters channel under the same rules, and
interrupting them is a core combat verb.

### Inscription

Where channeled spells are speech, inscription is **writing**: freezing a
spell into a physical object. An inscribed spell is a trigger-driven program
that runs without its author — a ward carved on a door, a trap etched into a
flagstone, a lantern that kindles itself at dusk.

Inscription is the late-game outlet for the prepared, engineering-minded
playstyle: instead of channeling under fire, you seed the battlefield ahead
of time and let your programs fight for you.

### Moment-to-Moment Play

- Real-time top-down movement, dodge, and a humble physical attack for when
  the mana runs dry.
- **Quick slots**: finished spells bind to buttons and cast (channel) with a
  press. The editor is for the workshop; combat is about choosing and timing
  what you have already written.
- **Procedural overworld**: noise-based biomes — forest, marsh, scrubland,
  mountains — each biased toward different substances and creatures, so
  different regions pose different tactical problems.
- **Procedural dungeons**: room-graph layouts with lock-and-key structure
  where the keys are language features. A door that needs four torches lit
  within a time window wants a loop; a corridor of pressure plates wants a
  ward.

### Progression at a Glance

| Tier | Editor | Constructs unlocked | Willpower (program size) | You can now… |
|------|--------|--------------------|--------------------------|--------------|
| 1 | Glyphs | selector + verb | 2–3 glyphs | fire a bolt at the nearest foe |
| 2 | Glyphs | sequencing, `if` | ~5 glyphs | shield yourself, then strike back |
| 3 | Glyphs | bounded loops, `let` | ~8 glyphs | volley spells, timed torch puzzles |
| 4 | Glyphs | wards/triggers | ~12 glyphs | contingencies: *when struck, push all foes back* |
| 5 | Script | named spells, parameters | pages, not glyphs | build a personal spellbook of reusable spells |
| 6 | Script | recursion, spells-as-values | — | chain lightning, metamagic, delayed casts |
| 7 | Script | inscription | — | leave your spells behind in the world |

## Build Plan

### Tech Stack

- **Language**: Haskell, GHC2024, cabal.
- **ECS**: [`apecs`](https://hackage.haskell.org/package/apecs) — a fast,
  mature entity-component-system with a monadic `System` API and
  per-component store selection (`Map`, `Unique`, `Global`).
- **Windowing, input, rendering**: [`sdl2`](https://hackage.haskell.org/package/sdl2)
  — window and renderer lifecycle, keyboard input, and immediate-mode 2D
  drawing via the SDL renderer; `sdl2-ttf` for text (in use since M3 for the
  glyph editor UI, rendering the pixel fonts from Franuka's UI pack — which
  lives in the untracked `assets/` directory and is credited in
  `CREDITS.md`). All SDL use stays behind the thin `Engine` module.
- **Parsing**: `megaparsec` for the textual Wyrdtongue.
- **Art**: placeholder colored quads and simple sprites until late (plus the
  third-party UI pack for fonts, and later menus/frames); the design does
  not depend on art quality.

### Milestones

Each milestone has a concrete "done when" so progress is checkable.

- **M0 — Skeleton.** Add dependencies; open an SDL window; render a quad
  moving under an apecs system.
  *Done when: a shape moves on screen at a stable tick rate.*
- **M1 — Player & world.** Tilemap rendering, player movement with
  collision, camera follow.
  *Done when: you can walk around a hand-authored map and bump into walls.*
- **M2 — Spell VM core.** The core AST and the ticked coroutine interpreter
  as an ECS system; mana; first effect verbs (`bolt`, `push`, `kindle`);
  hardcoded spells castable from quick slots. Build the VM before any
  editor — it is the riskiest piece and everything else hangs off it.
  *Done when: pressing a button channels a multi-instruction spell over
  several ticks and its effects land in the world.*
- **M3 — Block editor.** In-game glyph editor over the core AST; save/load
  spells; Willpower program-size budget enforced.
  *Done when: a spell assembled in-game casts from a quick slot.*
  *Shipped as*: a keyboard- and mouse-driven editor (`E` in play) showing
  the spell as indented glyph rows over a pure document model (`Glyph.hs`).
  Keyboard: palette hotkeys insert, arrows move cursor/field, `-`/`=` cycle
  values. Mouse (Scratch-style): drag blocks from the palette or between
  rows with a snap line at the nearest valid gap or hole, click a field to
  pick its value from a dropdown, drag a block onto the palette to delete
  its subtree; both input methods edit the same rows and stay in sync. The
  editable subset gives every block one child list (`if` is then-only for
  now), so a cursor path is a plain index list; the tier-2+ constructs
  (`if`, loops, `let`) are all present from the start rather than
  level-gated — gating arrives with M8. The spellbook persists to
  `spellbook.wyrd` via derived `Show`/`Read`, falling back per slot on bad
  data; Willpower (`willpowerMax`, currently a flat 8) is enforced at
  insert, commit, load, and cast.
- **M4 — Combat & backlash.** Enemies with simple AI, damage, and the
  interruption/fizzle/backlash rules for both player and enemy casters.
  *Done when: you can lose — and a stagger mid-cast visibly hurts you.*
  *Shipped as*: two enemy kinds on the overworld — melee **chasers** and
  **hexers** that channel a bolt volley through the *same* VM/cast pipeline
  as the player (slower `castPace`; the gold cast bar over their heads is
  the interrupt telegraph) — plus generic `Health`/`Faction` components
  (bolts carry their caster's faction; target dummies are just inert
  enemies now). Any hit on a channeling caster staggers the cast, and every
  collapse (stagger, no target, out of mana…) backlashes the caster for
  `backlashDamage`, scaling with the mana already committed — backlash
  deliberately bypasses the player's post-hit i-frames. Player death is a
  `GameOver` mode (YOU DIED veil, `R` respawns via the same `spawnLevel`
  as setup), not entity destruction. Feedback: white hit flash, i-frame
  blink, a full-screen red wash sized to the flash, in-world HP pips, and
  a red health bar in HUD slot 2. All numbers live in the Spell.hs
  tunables block.
- **M5 — Procgen.** Noise-based overworld chunks with biomes; room-graph
  dungeons with locks keyed to language features.
  *Done when: a fresh seed produces an explorable overworld and a
  completable dungeon whose puzzle requires a loop or ward.*
- **M6 — Text language.** Megaparsec parser for the script superset
  (definitions, parameters, recursion, spells-as-values) targeting the same
  VM; in-game text editor via `sdl2-ttf`.
  *Done when: the recursive `chain` example above parses, casts, and can be
  round-tripped from an old glyph spell.*
- **M7 — Inscription.** Inscribing spells into world objects with triggers.
  *Done when: an inscribed trap fires with the player standing idle.*
- **M8 — Progression.** Tier/level progression.
- **M9 — Polish.** Save/load, audio, balancing instruction costs, real art pass.

### Risks

- **Engine churn already happened once** (aztecs/GLFW/OpenGL → apecs/SDL2).
  Keep engine-facing code confined to the thin `Engine` module so any future
  migration stays cheap.
- **In-game editor UX** (both glyph and text) is the biggest unknown. M3
  shipped keyboard-driven rows, then grew Scratch-style mouse drag/snap,
  dropdown field menus, and drag-to-palette deletion on the same document
  model; expect to keep iterating — wards at tier 4 and the M6 text editor
  are still open questions.
- **Balance** of per-instruction mana costs, Willpower budgets, and backlash
  scaling will need sustained playtesting; keep the numbers in data, not
  code.
