# Offline Ray PPM Demo

An offline raytracing demo inspired by [`demo/ray_sdl`](../ray_sdl), but tuned for a single detailed still image instead of interactive playback.

## What it renders

- A fixed **2048×1536** still-life composition.
- A richer scene than the live demo:
  - eight spheres with mixed matte / reflective materials,
  - a checker floor,
  - rear and side studio walls,
  - sky contribution for miss rays.
- Higher per-pixel quality:
  - **25 deterministic samples per pixel**,
  - **up to 8 reflection bounces**,
  - shadowed key light plus a fill light.

The result is written as a binary `P6` PPM file.

## Build and run

```bash
bash demo/ray_ppm/build.sh
```

By default this writes:

```text
demo/ray_ppm/out/ray_still_life.ppm
```

You can also provide an explicit output path:

```bash
bash demo/ray_ppm/build.sh /tmp/ray_still_life.ppm
```

Metal backend on macOS:

```bash
bash demo/ray_ppm/metal_build.sh
```

The Metal build reuses the offline PPM harness and bootstraps a hidden SDL video context before initializing Metal, matching the working SDL-backed Metal demo more closely.

## Structure

- `ray_ppm.hyd` defines the offline raytracing kernel exported by Hydrangea.
- `main.cpp` calls the exported kernel once, tone-maps the luminance image, and writes a PPM.
- `build.sh` compiles the generated C kernel and the C++ harness, then renders the final image.

## Notes

This demo keeps the Hydrangea side grayscale, then performs final color grading in `main.cpp`. That matches the existing demo style while making it easy to tune the final look without changing the traced lighting kernel.
