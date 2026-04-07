# Ray Tracer Demo

An object-space ray tracer rendering an animated scene of three orbiting spheres above a checker floor.  Written in Hydrangea, compiled to C with OpenMP parallelisation, and displayed via SDL2.

## What you see

- A 1280×960 window (display scale 1–4×).
- **Camera** orbits the scene centre driven by the frame counter — a smooth figure-8-style path.
- **Three animated spheres**: a small red/violet sphere and two larger pale spheres, moving on sin/cos trajectories.
- **Checker floor**: a 100-unit checker plane at y = −150, lit by the same directional light.
- **Lighting**: each surface receives direct Lambertian diffuse plus ambient base colour, with up to 4 reflection bounces per pixel.  Shadow rays occlude the light.
- **Cool/warm palette**: luminance is colourised in `main.cpp` via `colorize_ray` into a dark navy → steel blue → warm cream gradient.

## Controls

| Key / Input | Action |
|---|---|
| Space | Pause / resume camera orbit |
| R | Reset camera to start position |
| 1 / 2 / 4 | Set display scale (1×, 2×, 4×) |
| Q / Escape | Quit |

## Algorithm

### State representation

Per-pixel ray state is threaded through `foldl_while` as a nested tuple:

```
((active: int, (origX: float, (origY: float, origZ: float))),
 ((dirX: float, (dirY: float, dirZ: float)),
  ((thrR: float, (thrG: float, thrB: float)),
   (outR: float, (outG: float, outB: float)))))
```

| Field | Meaning |
|---|---|
| `active` | 1 while this ray still contributes; 0 when escaped or exhausted |
| `orig` | Current ray origin in world space |
| `dir` | Current ray direction (unit vector) |
| `thr` | Throughput colour — multiplied by each surface's albedo; shrinks towards zero |
| `out` | Accumulated reflected colour from surfaces already hit |

A ray starts with `thr = (1,1,1)` and `out = (0,0,0)`.  Each bounce multiplies `thr` by the surface's albedo and adds the lit surface colour to `out`.  When `thr` falls below `1e-4` the ray is deactivated.

### `bounce_step time lightX lightY lightZ st dummy`

Performs one reflection bounce:

1. **Hit detection** — tests the ray against `sphere_t` (×3) and `plane_t` (×1) using the standard sphere-intersection formula and a ray-plane dot-product test.  `near_t = min(t1, t2, t3, t4)` picks the nearest hit.

2. **Surface normal** — for spheres computes `(hit − centre) / radius`; for the plane uses `(0, 1, 0)`.

3. **Base colour** — spheres are solid (red/violet, pale, pale); the plane uses a checker pattern derived from `floor(px / 100) + floor(pz / 100)`.

4. **Shadow test** — shoots a shadow ray from an offset point along the surface normal toward the light source and re-tests against all four primitives.  If occluded, direct lighting is zeroed.

5. **Lighting** — Lambertian diffuse `max(0, n·l) × 0.85` added to ambient base `0.3`.

6. **Reflection** — computes `r = d − 2(d·n)n`.  Throughput is scaled by the surface's `shine` factor (specular weight).  The next bounce uses the reflected direction and the updated throughput.

`foldl_while` iterates this step at most `maxBounces = 4` times, using a `while active > 0` predicate.

### `ray_render dummy_state frame`

1. Derives `time = frame × 0.04` and computes the camera position (`eyeX/Y/Z`) orbiting on sin/cos curves around the scene centre.
2. Builds orthonormal camera basis: forward = `normalize(target − eye)`, right = `normalize(cross(forward, (0,1,0)))`, up = `cross(right, forward)`.
3. For each pixel `(py, px)` computes the ray direction from the perspective projection (FOV = 1.15, aspect-corrected).
4. Initialises the ray state and calls `foldl_while` with `bounce_step`.
5. Returns the mean of the three accumulated colour channels, clamped to `[0, 1]`.

The entire `generate [outH, outW]` is parallelised by the compiler via `#pragma omp parallel for` — each pixel is independent.

## What fusion does

The ray tracer is entirely stateless per pixel; there are no shared intermediate arrays between pixels.  The key fusion story is in the per-bounce computation:

- Inside `bounce_step`, all geometry tests (`t1…t4`, shadow tests `st1…st4`) are independent `if`-branches that the compiler can evaluate without materialising arrays.
- The `generate [outH, outW]` pixel loop has no cross-pixel dependencies, so the compiler emits a trivially parallel OpenMP loop.

The result is a render where every pixel is computed independently and in parallel, with all intermediate geometry quantities staying in registers across the at-most-4 bounce iterations.

## Build

```bash
bash demo/ray_sdl/build.sh
./demo/ray_sdl/out/ray_sdl
```

Requires SDL2 and a C++17 compiler.  The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override the compiler.
