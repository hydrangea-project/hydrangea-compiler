#include <SDL.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

#ifdef USE_METAL
#include "voxel_live_metal.h"
#else
#include "voxel_live.h"
#endif

// ---------------------------------------------------------------------------
// Render dimensions — must match voxel_live.hyd constants
// ---------------------------------------------------------------------------
static constexpr int RENDER_W = 320;
static constexpr int RENDER_H = 240;

// ---------------------------------------------------------------------------
// Colormap: luminance → warm volcanic orange/gold palette
// ---------------------------------------------------------------------------
struct Vec3 { double x, y, z; };

static Vec3 operator+(Vec3 a, Vec3 b) { return {a.x+b.x, a.y+b.y, a.z+b.z}; }
static Vec3 operator*(double s, Vec3 v) { return {s*v.x, s*v.y, s*v.z}; }

static double clamp01(double x) { return std::clamp(x, 0.0, 1.0); }

static Vec3 lerp(Vec3 a, Vec3 b, double t) {
  return (1.0 - t) * a + t * b;
}

static double smoothstep(double e0, double e1, double x) {
  const double t = clamp01((x - e0) / std::max(1.0e-9, e1 - e0));
  return t * t * (3.0 - 2.0 * t);
}

// Luminance [0,1] → dark-blue background / orange-gold surface
static Vec3 colorize_voxel(double lum) {
  if (lum < 1.0e-6) return {0.04, 0.06, 0.14};  // empty background
  const double t   = std::pow(lum, 0.85);
  const Vec3 dark  = {0.05, 0.08, 0.16};
  const Vec3 mid   = {0.88, 0.52, 0.18};
  const Vec3 hot   = {1.00, 0.93, 0.80};
  if (t < 0.45)
    return lerp(dark, mid, smoothstep(0.0, 0.45, t));
  else
    return lerp(mid,  hot, smoothstep(0.45, 1.0, t));
}

// Pack Vec3 [0,1] into SDL ARGB8888
static Uint32 toARGB(Vec3 c, SDL_PixelFormat* fmt) {
  const Uint8 r = static_cast<Uint8>(std::clamp(255.0 * c.x, 0.0, 255.0));
  const Uint8 g = static_cast<Uint8>(std::clamp(255.0 * c.y, 0.0, 255.0));
  const Uint8 b = static_cast<Uint8>(std::clamp(255.0 * c.z, 0.0, 255.0));
  return SDL_MapRGB(fmt, r, g, b);
}


// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  int scale = 2;  // default 2× (640×480 window)

  SDL_Window* window = SDL_CreateWindow(
    "Voxel [Hydrangea]",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    RENDER_W * scale, RENDER_H * scale,
    SDL_WINDOW_RESIZABLE
  );
  if (!window) {
    SDL_Log("SDL_CreateWindow error: %s", SDL_GetError());
    SDL_Quit();
    return 1;
  }

  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1,
    SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (!renderer) {
    SDL_Log("SDL_CreateRenderer error: %s", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }
  SDL_RenderSetLogicalSize(renderer, RENDER_W * scale, RENDER_H * scale);

  SDL_Texture* tex = SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
    RENDER_W, RENDER_H);
  if (!tex) {
    SDL_Log("SDL_CreateTexture error: %s", SDL_GetError());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }

#ifdef USE_METAL
  {
    // Locate metallib next to the executable
    std::string exe_dir;
    {
      const char* base = SDL_GetBasePath();
      if (base) { exe_dir = base; SDL_free(const_cast<char*>(base)); }
    }
    std::string metallib = exe_dir + "voxel_live.metallib";
    if (hyd_metal_init(metallib.c_str()) != 0) {
      SDL_Log("Metal init failed");
      SDL_DestroyTexture(tex);
      SDL_DestroyRenderer(renderer);
      SDL_DestroyWindow(window);
      SDL_Quit();
      return 1;
    }
  }
#endif

  SDL_PixelFormat* pf = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);
  std::vector<Uint32> pixels(RENDER_W * RENDER_H);

  int64_t frame = 0;
  bool paused = false;

  SDL_Event ev;
  bool running = true;

  while (running) {
    // --- Event handling ---
    while (SDL_PollEvent(&ev)) {
      switch (ev.type) {
        case SDL_QUIT:
          running = false;
          break;

        case SDL_KEYDOWN:
          switch (ev.key.keysym.sym) {
            case SDLK_q:
            case SDLK_ESCAPE:
              running = false;
              break;
            case SDLK_r:
              frame = 0;
              break;
            case SDLK_SPACE:
              paused = !paused;
              break;
            case SDLK_1:
              scale = 1;
              SDL_SetWindowSize(window, RENDER_W * scale, RENDER_H * scale);
              SDL_RenderSetLogicalSize(renderer, RENDER_W * scale, RENDER_H * scale);
              break;
            case SDLK_2:
              scale = 2;
              SDL_SetWindowSize(window, RENDER_W * scale, RENDER_H * scale);
              SDL_RenderSetLogicalSize(renderer, RENDER_W * scale, RENDER_H * scale);
              break;
            case SDLK_4:
              scale = 4;
              SDL_SetWindowSize(window, RENDER_W * scale, RENDER_H * scale);
              SDL_RenderSetLogicalSize(renderer, RENDER_W * scale, RENDER_H * scale);
              break;
          }
          break;
      }
    }

    // --- Render one voxel frame ---
    // dummy_state is unused in the kernel (typed int64_t by inference), pass 0.
    // Pass current frame counter; advance only when not paused.
#ifdef USE_METAL
    hyd_pair_aa_t result = hyd_metal_voxel_render(0LL, frame);
#else
    hyd_pair_aa_t result = hyd_export_voxel_render(0LL, frame);
#endif
    if (!paused) ++frame;

    // --- Colorize luminance → ARGB pixels ---
    // result.fst = luminance array [RENDER_H × RENDER_W], result.snd = tiny dummy
    {
      const double* lum = hyd_array_data_f64(result.fst);
      for (int i = 0; i < RENDER_H; ++i) {
        for (int j = 0; j < RENDER_W; ++j) {
          pixels[i * RENDER_W + j] = toARGB(colorize_voxel(lum[i * RENDER_W + j]), pf);
        }
      }
    }
    hyd_array_free(result.fst);
    hyd_array_free(result.snd);

    SDL_UpdateTexture(tex, nullptr, pixels.data(), RENDER_W * static_cast<int>(sizeof(Uint32)));
    SDL_RenderClear(renderer);
    SDL_Rect dst{ 0, 0, RENDER_W * scale, RENDER_H * scale };
    SDL_RenderCopy(renderer, tex, nullptr, &dst);
    SDL_RenderPresent(renderer);

    char title[128];
    std::snprintf(title, sizeof(title),
      "Voxel [Hydrangea]  frame=%lld  scale=%dx%s",
      frame, scale, paused ? "  [PAUSED]" : "");
    SDL_SetWindowTitle(window, title);
  }

#ifdef USE_METAL
  hyd_metal_cleanup();
#endif
  // No persistent state arrays to free
  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
