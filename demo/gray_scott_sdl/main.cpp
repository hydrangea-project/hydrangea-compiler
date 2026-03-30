#include <SDL.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

#include "gray_scott_live.h"

// ---------------------------------------------------------------------------
// Grid dimensions — must match gray_scott_live.hyd constants
// ---------------------------------------------------------------------------
static constexpr int GRID_H = 256;
static constexpr int GRID_W = 256;

// ---------------------------------------------------------------------------
// Colormap (ported from demo/gray_scott/main.cpp)
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

// V concentration [0, 0.5] → navy / teal / bright-yellow
static Vec3 colorize_gs(double v) {
  const double t = clamp01(v / 0.5);
  const Vec3 dark   {0.02, 0.04, 0.12};
  const Vec3 mid    {0.05, 0.55, 0.65};
  const Vec3 bright {0.98, 0.92, 0.20};
  if (t < 0.5)
    return lerp(dark, mid, smoothstep(0.0, 0.5, t));
  else
    return lerp(mid, bright, smoothstep(0.5, 1.0, t));
}

// Pack Vec3 [0,1] into SDL ARGB8888
static Uint32 toARGB(Vec3 c, SDL_PixelFormat* fmt) {
  const Uint8 r = static_cast<Uint8>(std::clamp(255.0 * c.x, 0.0, 255.0));
  const Uint8 g = static_cast<Uint8>(std::clamp(255.0 * c.y, 0.0, 255.0));
  const Uint8 b = static_cast<Uint8>(std::clamp(255.0 * c.z, 0.0, 255.0));
  return SDL_MapRGB(fmt, r, g, b);
}

// ---------------------------------------------------------------------------
// Simulation state helpers
// ---------------------------------------------------------------------------
static hyd_pair_aa_t make_initial_state() {
  hyd_tuple_t shape = hyd_tuple_make(2, (int64_t)GRID_H, (int64_t)GRID_W);
  hyd_array_t* u = hyd_array_alloc_bytes(shape, sizeof(double));
  hyd_array_t* v = hyd_array_alloc_bytes(shape, sizeof(double));

  double* ud = hyd_array_data_f64(u);
  double* vd = hyd_array_data_f64(v);

  for (int i = 0; i < GRID_H; ++i) {
    for (int j = 0; j < GRID_W; ++j) {
      const int idx = i * GRID_W + j;
      ud[idx] = 1.0;
      const int di = i - GRID_H / 2;
      const int dj = j - GRID_W / 2;
      const bool in_disk = (di*di + dj*dj) < (20*20);
      const bool seed    = ((i * 251 + j * 127) % 100) < 2;
      vd[idx] = (in_disk || seed) ? 0.5 : 0.0;
    }
  }

  return hyd_pair_aa_t{ u, v };
}

// Inject V=0.5 in a circle of given radius at grid position (gx, gy)
static void perturb(hyd_pair_aa_t& state, int gx, int gy, int radius) {
  double* vd = hyd_array_data_f64(state.snd);
  for (int i = std::max(0, gy - radius); i < std::min(GRID_H, gy + radius + 1); ++i) {
    for (int j = std::max(0, gx - radius); j < std::min(GRID_W, gx + radius + 1); ++j) {
      const int di = i - gy, dj = j - gx;
      if (di*di + dj*dj <= radius*radius)
        vd[i * GRID_W + j] = 0.5;
    }
  }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  int scale = 2;  // default 2× (512×512 window)

  SDL_Window* window = SDL_CreateWindow(
    "Gray-Scott Live [Hydrangea]",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    GRID_W * scale, GRID_H * scale,
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
  SDL_RenderSetLogicalSize(renderer, GRID_W * scale, GRID_H * scale);

  // Streaming texture: one pixel per grid cell (scaled by SDL_RenderCopy)
  SDL_Texture* tex = SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
    GRID_W, GRID_H);
  if (!tex) {
    SDL_Log("SDL_CreateTexture error: %s", SDL_GetError());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }

  SDL_PixelFormat* pf = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);

  // Pixel buffer for texture upload
  std::vector<Uint32> pixels(GRID_W * GRID_H);

  // Simulation state
  hyd_pair_aa_t state = make_initial_state();
  long long total_steps = 0;
  int steps_per_frame = 5;
  bool paused = false;
  bool mouse_down = false;

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
            case SDLK_r: {
              hyd_array_free(state.fst);
              hyd_array_free(state.snd);
              state = make_initial_state();
              total_steps = 0;
              break;
            }
            case SDLK_SPACE:
              paused = !paused;
              break;
            case SDLK_1:
              scale = 1;
              SDL_SetWindowSize(window, GRID_W * scale, GRID_H * scale);
              SDL_RenderSetLogicalSize(renderer, GRID_W * scale, GRID_H * scale);
              break;
            case SDLK_2:
              scale = 2;
              SDL_SetWindowSize(window, GRID_W * scale, GRID_H * scale);
              SDL_RenderSetLogicalSize(renderer, GRID_W * scale, GRID_H * scale);
              break;
            case SDLK_4:
              scale = 4;
              SDL_SetWindowSize(window, GRID_W * scale, GRID_H * scale);
              SDL_RenderSetLogicalSize(renderer, GRID_W * scale, GRID_H * scale);
              break;
            case SDLK_PLUS:
            case SDLK_EQUALS:
              steps_per_frame = std::min(50, steps_per_frame + 1);
              break;
            case SDLK_MINUS:
              steps_per_frame = std::max(1, steps_per_frame - 1);
              break;
          }
          break;

        case SDL_MOUSEBUTTONDOWN:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = true;
            const int gx = ev.button.x / scale;
            const int gy = ev.button.y / scale;
            perturb(state, gx, gy, 15);
          }
          break;

        case SDL_MOUSEBUTTONUP:
          if (ev.button.button == SDL_BUTTON_LEFT)
            mouse_down = false;
          break;

        case SDL_MOUSEMOTION:
          if (mouse_down) {
            const int gx = ev.motion.x / scale;
            const int gy = ev.motion.y / scale;
            perturb(state, gx, gy, 15);
          }
          break;
      }
    }

    // --- Simulation steps ---
    if (!paused) {
      for (int k = 0; k < steps_per_frame; ++k) {
        hyd_pair_aa_t next = hyd_export_gs_step(state, 0LL);
        hyd_array_free(state.fst);
        hyd_array_free(state.snd);
        state = next;
      }
      total_steps += steps_per_frame;
    }

    // --- Render ---
    const double* vd = hyd_array_data_f64(state.snd);
    for (int i = 0; i < GRID_H; ++i) {
      for (int j = 0; j < GRID_W; ++j) {
        pixels[i * GRID_W + j] = toARGB(colorize_gs(vd[i * GRID_W + j]), pf);
      }
    }

    SDL_UpdateTexture(tex, nullptr, pixels.data(), GRID_W * static_cast<int>(sizeof(Uint32)));

    SDL_RenderClear(renderer);
    SDL_Rect dst{ 0, 0, GRID_W * scale, GRID_H * scale };
    SDL_RenderCopy(renderer, tex, nullptr, &dst);
    SDL_RenderPresent(renderer);

    // Update title with stats
    char title[128];
    std::snprintf(title, sizeof(title),
      "Gray-Scott Live [Hydrangea]  step=%lld  spf=%d  scale=%dx%s",
      total_steps, steps_per_frame, scale, paused ? "  [PAUSED]" : "");
    SDL_SetWindowTitle(window, title);
  }

  // Cleanup
  hyd_array_free(state.fst);
  hyd_array_free(state.snd);
  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
