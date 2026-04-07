# Particle-in-Cell Density Demo

An interactive 2D particle simulation that accumulates mass density onto a grid each frame, with the density rendered as a colour heat map.  Written in Hydrangea, compiled to C, and displayed via SDL2.

## What you see

- A 512×512 window (128×128 grid cells, 4× display scale).
- **Colour heat map**: each grid cell is coloured by how many particles currently occupy it.  Low density is dark blue; mid density is teal/green; high density is bright yellow; peak density is white.  A sqrt perceptual scale is applied so even sparsely-visited cells register visible colour.
- **White dots**: the 400 individual particles, drawn as small 3×3 circles.
- **Red crosshair**: the attractor position.  Click and drag to move it.

## Controls

| Key / Input | Action |
|---|---|
| Left mouse drag | Move the attractor |
| Space | Pause / resume |
| R | Reset particles to initial ring |
| + / - | Increase / decrease simulation steps per frame |
| Q / Escape | Quit |

## Algorithm

### State representation

All particle data is packed into a single flat array of length `num_particles * 4 = 1600`.  Element `p * 4 + c` holds component `c` of particle `p`:

| Component | Meaning |
|---|---|
| 0 | x position (grid coordinates) |
| 1 | y position (grid coordinates) |
| 2 | x velocity |
| 3 | y velocity |

Initial conditions place all particles on a slightly jittered ring around the grid centre, each given a tangential velocity so they orbit before the attractor pulls them into new trajectories.

### Simulation step (`pic_step state attractor_encoded`)

Each call to `pic_step` performs one simulation step and returns `(new_state, density_grid)`.

**Input**
- `state`: the current `[1600]` particle array.
- `attractor_encoded`: the attractor position packed as `ax * 10000 + ay` (integer grid coordinates).

**Output**
- `new_state`: updated `[1600]` particle array.
- `density_grid`: a flat `[16384]` array (128×128) of accumulated particle counts.

#### 1. Euler integration

```
generate [state_size] f
```

Produces all 1600 output elements in parallel.  For each particle `p`, the body:

1. Reads the current position `(px, py)` and velocity `(vx, vy)`.
2. Computes the displacement to the attractor and applies a softened inverse-square gravitational force: `force = G / (dist² + ε)`.
3. Integrates velocity (`damping * (v + accel * dt)`) and then position (`pos + vel * dt`).
4. The four updated components (`npx`, `npy`, `nvx`, `nvy`) are selected by `c = k mod 4`.

#### 2. Density accumulation via guarded scatter

```
scatter_guarded (+.) (fill [num_cells] 0.0)
  (map cellFn  particles)
  (map massFn  particles)
  (map keepFn  particles)
```

For each particle `p`, three functions are evaluated:

- **`cellFn p`**: converts the particle's grid position to a flat cell index `floor(py) * grid_w + floor(px)`.
- **`massFn p`**: returns `1.0` (unit mass per particle).
- **`keepFn p`**: returns `true` if the particle lies within the grid boundary `[0, grid_w) × [0, grid_h)`; `false` otherwise (the particle's contribution is silently discarded).

The scatter atomically adds each particle's mass into its target cell, building the density grid from scratch every frame.

## What fusion does

The three arguments to `scatter_guarded` — bin indices, weights, and guards — are each a `map` applied to the same source array (`particles`, a simple index range).  The Hydrangea compiler identifies this pattern as **Case 6a scatter_reindex** and fuses all three maps into the scatter loop body.

**Without fusion**, the compiler would allocate and fill three temporary arrays of length 400 before the scatter:

| Intermediate array | Contents |
|---|---|
| `map cellFn particles` | target cell index per particle |
| `map massFn particles` | mass weight per particle (all 1.0) |
| `map keepFn particles` | boundary guard per particle |

**With fusion**, none of these arrays is materialised.  Instead the scatter loop body computes `cellFn`, `massFn`, and `keepFn` inline for each particle as it processes it.  The only memory written is the `[16384]` density grid itself.

The result: density accumulation for 400 particles touches only the output grid (no temporary allocation), and each particle's position is read exactly once from the state array.

## Build

```bash
bash demo/pic_sdl/build.sh
./demo/pic_sdl/out/pic_sdl
```

Requires SDL2 and a C++17 compiler.  The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override the compiler.
