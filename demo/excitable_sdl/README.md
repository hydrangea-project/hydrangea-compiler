# Excitable Media Demo

An interactive SDL demo for Hydrangea built around a batched 2D stencil kernel. It renders a cyclic excitable-media cellular automaton that produces spiral waves, target-like rings, and mouse-seeded disturbances in a subdued dark palette.

## What you see

- A 320x320 simulation texture with display scale 1x, 2x, or 4x.
- A cyclic phase field colorized with muted dark blues, teals, and violets.
- Self-organizing wavefronts seeded from a few deterministic phase-ramp sources on a calm background.
- Mouse-painted disturbances that inject new rotating defects and rings.

## Controls

| Key / Input | Action |
|---|---|
| Left mouse drag | Paint a phase-ramp disturbance |
| Space | Pause / resume animation |
| R | Reset to the seeded initial pattern |
| 1 / 2 / 4 | Set display scale |
| + / - | Increase / decrease steps per frame |
| Q / Escape | Quit |

## Algorithm

The Hydrangea kernel exports:

```hydrangea
excitable_frame : Array[grid_h, grid_w, int] -> int -> Array[grid_h, grid_w, int]
```

The second argument is a compact encoded control word carrying:

1. brush x
2. brush y
3. brush phase
4. brush active flag
5. substeps per frame

Each call performs:

1. **Brush injection**: if the mouse is down, a small disk around the brush position is overwritten with a phase ramp.
2. **Batched stepping**: the kernel runs `substeps` iterations internally with `foldl`, so the time loop lives inside Hydrangea instead of the SDL harness.

Each substep is intentionally split into two logical stencil stages:

1. **Neighbour tally**: for each interior cell, count how many of the 8 Moore neighbours already have the cell's successor phase.
2. **Transition**: if at least one neighbour already has the cell's successor phase, advance the cell by one phase modulo `phase_count`; otherwise leave it unchanged.

The border is kept fixed so the core 3x3 stencil uses direct `i +/- 1`, `j +/- 1` accesses without periodic wraparound. That keeps the main recurrence closer to the affine stencil shape we want for future polyhedral work.

## Why this is a useful future polyhedral target

- The main work is a real 2D stencil over a time-stepped phase field.
- The time loop is inside Hydrangea via `foldl`, which is the right shape for future schedule-based transformations.
- The substep is deliberately staged as "count neighbours" then "apply transition", giving future fusion/distribution work something meaningful to optimize.
- The fixed-border interior update is closer to the kind of stencil kernel that future skewing and wavefront schedules should handle.

## Build

```bash
bash demo/excitable_sdl/build.sh
./demo/excitable_sdl/out/excitable_sdl
```

Requires SDL2 and a C++17 compiler. The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override compiler selection.
