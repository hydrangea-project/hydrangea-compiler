# Scan Light Sweep Demo

An interactive visual demo for Hydrangea's extended prefix-scan family. The frame is built from directional light transport fields computed with `scan`, `scan_inclusive`, `scanr`, and `scanr_inclusive`, then colorized in real time with SDL2.

## What you see

- A 256×256 simulation texture (display scale 1–4×).
- A moving source that sweeps horizontally while oscillating vertically.
- A mouse-controlled local emitter (left-drag).
- Directional color structure:
  - horizontal scan imbalance drives warm/red tones
  - vertical scan imbalance drives cool/blue tones
  - combined scan energy drives brightness/green

## Controls

| Key / Input | Action |
|---|---|
| Left mouse drag | Move local emitter |
| Space | Pause / resume animation |
| R | Reset frame counter + emitter position |
| 1 / 2 / 4 | Set display scale |
| + / - | Increase / decrease steps per frame |
| Q / Escape | Quit |

## Algorithm

The Hydrangea kernel exports:

```hydrangea
scan_frame : int -> int -> Array[num_cells, int]
```

Inputs:
- `frame`: animation step.
- `mouse_encoded`: `mx * 10000 + my`.

For each cell, a source term is generated from:
1. an animated moving emitter, and
2. a mouse emitter.

An attenuation field modulates transport. The kernel then performs scan transport over row-reset linearization using:

1. `scan` (exclusive left→right),
2. `scan_inclusive` (inclusive left→right),
3. `scanr` (exclusive right→left),
4. `scanr_inclusive` (inclusive right→left).

Per-row boundaries are enforced by encoding reset markers in each scan element, so each scan restarts at row edges despite flat storage.

The four resulting transport fields are combined into:
- scalar energy (brightness),
- signed directional contrast (warm/cool shift),

then packed to 24-bit RGB integers for the SDL harness.

## Build

```bash
bash demo/scan_sdl/build.sh
./demo/scan_sdl/out/scan_sdl
```

Requires SDL2 and a C++17 compiler. The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override compiler selection.
