# Voxel Renderer Demo

An interactive voxel volume renderer showing a solid sphere and a torus floating in space, rendered as a 64×64×48 voxel grid.  Written in Hydrangea, compiled to C with OpenMP parallelisation, and displayed via SDL2.

## What you see

- A 640×480 window (default 2× scale, resizable to 1×/2×/4×).
- **Camera** orbits the scene centre driven by the frame counter — a smooth circular path.
- **Two voxelised objects**: a solid sphere (radius 14, centred near the front-left of the grid) and a torus (ring radius 10, centred near the front-right).  Both are constructed from a 200,000-point cloud with trilinear weighting so interior voxels carry partial mass.
- **Orange/gold palette**: empty space is dark navy; occupied voxels are colourised in `main.cpp` via `colorize_voxel` into a dark blue → volcanic orange → pale gold gradient keyed on luminance.
- No shadows, no specular — surface detection plus diffuse and rim lighting only.

## Controls

| Key / Input | Action |
|---|---|
| Space | Pause / resume camera orbit |
| R | Reset camera to start position |
| 1 / 2 / 4 | Set display scale (1×, 2×, 4×) |
| Q / Escape | Quit |

## Algorithm

### State representation

Two distinct layers:

**Layer 1 — voxel scene** (computed once, cached for all subsequent frames):

```
voxel_scene_fast :: [nz=48, ny=64, nx=64] float
```

Built from a 200,000-point cloud.  Each point contributes to 8 voxel corners via trilinear weights, accumulated into the flat `[nvox = 64×64×48]` array with a `scatter_guarded`.

**Layer 2 — per-pixel ray state** (threaded through `foldl_while`):

```
((hit: int, shade: float),
 ((t: float, prevT: float),
  (prevDensity: float, havePrev: int)))
```

| Field | Meaning |
|---|---|
| `hit` | 1 once surface crossing detected; stops the march |
| `shade` | Computed lighting value (diffuse + rim) |
| `t` | Current ray parameter |
| `prevT` | Previous `t` value |
| `prevDensity` | Density sampled at `prevT` |
| `havePrev` | 1 once at least one sample has been taken |

### Scene build (`voxel_scene_fast`)

```
scatter_guarded (+.) (fill [nvox] 0.0)
  (generate [contribs] route_of)
  (generate [contribs] weight_of)
  (generate [contribs] keep_of)
```

- `contribs = n × 8 = 1,600,000` — each of the 200K points contributes to all 8 corners of the voxel it falls in.
- **`route_of`**: maps each contribution index to a flat voxel cell index `(z·ny + y)·nx + x`.
- **`weight_of`**: computes the trilinear interpolation weight `(1−bx)(4−fx)·(1−by)(4−fy)·(1−bz)(4−fz) / 64` for smooth voxel boundaries.
- **`keep_of`**: returns `true` only for points that lie inside one of the two objects (sphere or torus cross-section).
- **`scatter_guarded`**: atomically accumulates each contribution into the target cell, discarding any that fall outside the voxel grid boundary.

The result is a `[48, 64, 64]` voxel array where each cell holds the sum of trilinear weights from all nearby points — effectively a smooth density field.

### Trilinear interpolation (`tri voxels nx ny nz xf yf zf`)

Samples the voxel field at continuous float coordinates `(xf, yf, zf)` using the eight surrounding voxel corners:

```
c000 ── c100   tx = xf − floor(xf)
│    ╲  │      ty = yf − floor(yf)
c010 ── c110   tz = zf − floor(zf)
    ╲  ╱
c001 ── c101   c0  = c000·(1−tx) + c100·tx
               c1  = c010·(1−tx) + c110·tx
               c   = c0·(1−ty)  + c1·ty
               result = c·(1−tz) + c1·tz
```

Boundary conditions clamp to `[0, nx/ny/nz−1]`.  Out-of-bounds queries return `0.0` via `check_index`.

### Ray march (`ray_step voxels nx ny nz ... acc dummy`)

1. If `hit > 0` the surface has already been found — return state unchanged.
2. Sample `density = tri voxels ... px py pz` at the current ray point.
3. **Surface crossing detection**: if `prevDensity < threshold` and `density >= threshold`, the ray has entered the surface.  Triggers shading computation.
4. **Shading** (only on crossing):
   - Compute gradient normal via central differences using 6 neighbouring voxel samples.
   - Diffuse: `max(0, n·l)`.
   - Rim: `(1 − n·(−d))²` — silhouette edge brightening.
   - `shade = 0.20 + 0.80·diffuse + 0.15·rim`.
5. Advance `t += stepSize (0.4)` and loop until `t > tExit` or `hit = 1`.

### `voxel_render dummy_state frame`

1. Camera angle = `frame × 0.025`, orbit radius = `1.6 × nx`.  Camera always looks at the scene centre.
2. Derive orthonormal camera basis (forward, right, up) from the orbit position.
3. **Ray-box slab intersection**: compute `tEnter` and `tExit` for the voxel grid AABB along the ray direction (3-axis slab method).  If `tEnter > tExit` the ray misses.
4. Initialise `ray_state = ((0, 0.0), ((tStart, tStart), (0.0, 0)))` and call `foldl_while` with `ray_step` for up to `maxSteps = 200` iterations.
5. Return the `shade` component of the final state.

## What fusion does

### Cached scene (`voxel_scene_fast`)

`voxel_scene_fast` is a top-level `let` binding compiled by Hydrangea as a **cached lazy value**.  On the first call to `voxel_render` the compiler evaluates the scatter and produces the `[48, 64, 64]` voxel array.  That array is stored in the generated C's static allocation.  Every subsequent frame reuses the same array — the scatter is never re-executed.

Without this caching, the 1.6 million contributions would be re-scattered every frame.  With caching, the scene build cost is paid once at startup.

### Scatter fusion

The three `generate` calls feeding the scatter are each simple index-range → computed-value functions.  The compiler identifies this as the **scatter_reindex** pattern (Case 6a) and fuses all three into the scatter loop body — no intermediate arrays are materialised.

### Pixel loop parallelisation

The `generate [outH, outW]` pixel loop is the parallelisation point.  Each pixel writes to a disjoint output element, and the voxel scene is read-only.  The compiler emits `#pragma omp parallel for` over the flat pixel index space; threads share the cached voxel array read-only.

## Build

```bash
bash demo/voxel_sdl/build.sh
./demo/voxel_sdl/out/voxel_sdl
```

Requires SDL2 and a C++17 compiler.  The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override the compiler.
