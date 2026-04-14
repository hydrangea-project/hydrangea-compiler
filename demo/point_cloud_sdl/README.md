# Stanford Bunny Point Cloud Demo

An SDL demo that renders the classic Stanford bunny as a point cloud by projecting each point to the screen and compositing per-pixel contributions with different monoids. Written in Hydrangea, compiled to C, and displayed via SDL2.

## What you see

- A `768×768` render target displayed in a `1×` window by default.
- The Stanford bunny point cloud, sourced at build time from Stanford's official archive and normalized into a CSV consumed by the Hydrangea kernel.
- Three resolve modes over the same projected points:
  - **min**: nearest point wins, giving a crisp front-most silhouette.
  - **+**: additive accumulation, showing density as brightness.
  - **max**: farthest point wins, with a depth-based cyan fog/tint for a stronger x-ray feel.

## Controls

| Key / Input | Action |
|---|---|
| Left mouse drag | Rotate the cloud |
| `1` | Resolve with `min` |
| `2` | Resolve with `+` |
| `3` | Resolve with `max` |
| `Tab` / `M` | Cycle resolve mode |
| `R` | Reset rotation + mode |
| `Q` / `Escape` | Quit |

## Algorithm

The Hydrangea kernel exports:

```hydrangea
point_cloud_frame : int -> int -> int -> Array[num_pixels, int]
```

Inputs:
- `yaw_milli`: yaw in milliradians.
- `pitch_milli`: pitch in milliradians.
- `mode`: `0 = min`, `1 = +`, `2 = max`.

For each point index in the Stanford bunny point cloud:

1. Read its normalized object-space position from a generated CSV.
2. Rotate it by yaw and pitch.
3. Perspective-project it to a pixel.
4. Guard on `0 <= x < width` and `0 <= y < height`.

### Resolve modes

#### `min`

Each point contributes a packed integer:

```text
packed = depth_key * 2^24 + rgb24
```

`scatter_guarded min` chooses the smallest packed value per pixel, so the nearest depth wins and carries its 24-bit RGB colour with it.

#### `+`

Each point contributes `1` into a per-pixel counter using `scatter_guarded (+)`. The final count is mapped to grayscale brightness, so dense overlaps appear brighter.

#### `max`

Uses the same packed representation as `min`, but with `scatter_guarded max`, so the farthest point wins. Before packing, the winning colour is pushed toward a cool cyan fog as depth increases, making `max` read more like an x-ray pass than a simple front/back swap.

## Build

```bash
bash demo/point_cloud_sdl/build.sh
./demo/point_cloud_sdl/out/point_cloud_sdl
```

Requires SDL2, `curl`, Python, and a C++17 compiler. The build script auto-detects `sdl2-config`; set `DEMO_CC` / `DEMO_CXX` to override compiler selection.

On first build, the script downloads `bunny.tar.gz` from the Stanford Computer Graphics Laboratory, extracts `bun_zipper_res2.ply`, normalizes the vertex positions, and writes `demo/point_cloud_sdl/out/bunny_points.csv`. The executable now switches to its own directory on startup, so it can find that CSV even when launched outside the repo root.

## Source and license note

The bunny data is fetched from the official Stanford 3D Scanning Repository rather than vendored into this repo. The Stanford site permits research use and free redistribution with attribution, but notes that the models are not for commercial use without permission. See the repository page for the exact terms:

- <https://graphics.stanford.edu/data/3Dscanrep/>
