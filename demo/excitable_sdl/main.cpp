#include <SDL.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <vector>

#include "excitable_live.h"

static constexpr int GRID_H = 320;
static constexpr int GRID_W = 320;
static constexpr int PHASE_COUNT = 24;

struct Vec3 {
  double x;
  double y;
  double z;
};

static double clamp01(double x) {
  return std::clamp(x, 0.0, 1.0);
}

static Vec3 lerp(Vec3 a, Vec3 b, double t) {
  return {
    a.x + (b.x - a.x) * t,
    a.y + (b.y - a.y) * t,
    a.z + (b.z - a.z) * t,
  };
}

static Uint32 toARGB(Vec3 c, SDL_PixelFormat* fmt) {
  const Uint8 r = static_cast<Uint8>(std::clamp(255.0 * c.x, 0.0, 255.0));
  const Uint8 g = static_cast<Uint8>(std::clamp(255.0 * c.y, 0.0, 255.0));
  const Uint8 b = static_cast<Uint8>(std::clamp(255.0 * c.z, 0.0, 255.0));
  return SDL_MapRGB(fmt, r, g, b);
}

static int clamp_i(int x, int lo, int hi) {
  return std::max(lo, std::min(x, hi));
}

static int ramp_phase(int x, int y, int cx, int cy) {
  return (2 * std::abs(x - cx) + 3 * std::abs(y - cy)) % PHASE_COUNT;
}

static hyd_array_t* make_initial_state() {
  hyd_tuple_t shape = hyd_tuple_make(2, static_cast<int64_t>(GRID_H), static_cast<int64_t>(GRID_W));
  hyd_array_t* state = hyd_array_alloc_bytes(shape, sizeof(int64_t));
  int64_t* phase = hyd_array_data_int64(state);

  for (int i = 0; i < GRID_H; ++i) {
    for (int j = 0; j < GRID_W; ++j) {
      const int idx = i * GRID_W + j;
      int value = 0;

      if (i <= 0 || j <= 0 || i >= GRID_H - 1 || j >= GRID_W - 1) {
        value = 0;
      }

      const int d0x = j - GRID_W / 3;
      const int d0y = i - GRID_H / 2;
      if (d0x * d0x + d0y * d0y < 26 * 26) {
        value = ramp_phase(j, i, GRID_W / 3, GRID_H / 2);
      }

      const int d1x = j - (2 * GRID_W) / 3;
      const int d1y = i - GRID_H / 3;
      if (d1x * d1x + d1y * d1y < 22 * 22) {
        value = ramp_phase(j, i, (2 * GRID_W) / 3, GRID_H / 3);
      }

      const int d2x = j - GRID_W / 2;
      const int d2y = i - (3 * GRID_H) / 4;
      if (d2x * d2x + d2y * d2y < 18 * 18) {
        value = (7 + ramp_phase(j, i, GRID_W / 2, (3 * GRID_H) / 4)) % PHASE_COUNT;
      }

      phase[idx] = value;
    }
  }

  return state;
}

static Vec3 colorize_phase(int64_t phase) {
  const double t = static_cast<double>(phase % PHASE_COUNT) / static_cast<double>(PHASE_COUNT);
  const double band = t * 4.0;
  const int segment = static_cast<int>(std::floor(band)) % 4;
  const double local_t = band - std::floor(band);

  const Vec3 c0{0.02, 0.03, 0.06};
  const Vec3 c1{0.05, 0.08, 0.12};
  const Vec3 c2{0.03, 0.10, 0.09};
  const Vec3 c3{0.08, 0.05, 0.10};

  Vec3 base = c0;
  if (segment == 0) {
    base = lerp(c0, c1, local_t);
  } else if (segment == 1) {
    base = lerp(c1, c2, local_t);
  } else if (segment == 2) {
    base = lerp(c2, c3, local_t);
  } else {
    base = lerp(c3, c0, local_t);
  }

  const double accent = (phase < 2) ? 0.025 : (phase < 5 ? 0.012 : 0.0);
  return {
    clamp01(base.x + accent),
    clamp01(base.y + accent),
    clamp01(base.z + accent * 0.8),
  };
}

static int64_t encode_control(
  int mouse_x,
  int mouse_y,
  int brush_phase,
  bool brush_on,
  int substeps
) {
  return static_cast<int64_t>(mouse_x)
    + static_cast<int64_t>(GRID_W) * (
        static_cast<int64_t>(mouse_y)
        + static_cast<int64_t>(GRID_H) * (
            static_cast<int64_t>(brush_phase)
            + static_cast<int64_t>(PHASE_COUNT) * (
                static_cast<int64_t>(brush_on ? 1 : 0) + 2LL * static_cast<int64_t>(substeps))));
}

int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  int scale = 2;
  int steps_per_frame = 2;
  int frame = 0;
  int mouse_x = GRID_W / 2;
  int mouse_y = GRID_H / 2;
  bool paused = false;
  bool mouse_down = false;

  SDL_Window* window = SDL_CreateWindow(
    "Excitable Media [Hydrangea]",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    GRID_W * scale, GRID_H * scale,
    SDL_WINDOW_RESIZABLE
  );
  if (!window) {
    SDL_Log("SDL_CreateWindow error: %s", SDL_GetError());
    SDL_Quit();
    return 1;
  }

  SDL_Renderer* renderer = SDL_CreateRenderer(
    window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (!renderer) {
    SDL_Log("SDL_CreateRenderer error: %s", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }
  SDL_RenderSetLogicalSize(renderer, GRID_W * scale, GRID_H * scale);

  SDL_Texture* tex = SDL_CreateTexture(
    renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, GRID_W, GRID_H);
  if (!tex) {
    SDL_Log("SDL_CreateTexture error: %s", SDL_GetError());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }

  SDL_PixelFormat* pf = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);
  std::vector<Uint32> pixels(GRID_W * GRID_H);
  hyd_array_t* state = make_initial_state();

  SDL_Event ev;
  bool running = true;

  while (running) {
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
            case SDLK_SPACE:
              paused = !paused;
              break;
            case SDLK_r:
              hyd_array_free(state);
              state = make_initial_state();
              frame = 0;
              mouse_x = GRID_W / 2;
              mouse_y = GRID_H / 2;
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
              steps_per_frame = std::min(24, steps_per_frame + 1);
              break;
            case SDLK_MINUS:
              steps_per_frame = std::max(1, steps_per_frame - 1);
              break;
          }
          break;

        case SDL_MOUSEBUTTONDOWN:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = true;
            mouse_x = clamp_i(ev.button.x / scale, 0, GRID_W - 1);
            mouse_y = clamp_i(ev.button.y / scale, 0, GRID_H - 1);
          }
          break;

        case SDL_MOUSEBUTTONUP:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = false;
          }
          break;

        case SDL_MOUSEMOTION:
          if (mouse_down) {
            mouse_x = clamp_i(ev.motion.x / scale, 0, GRID_W - 1);
            mouse_y = clamp_i(ev.motion.y / scale, 0, GRID_H - 1);
          }
          break;
      }
    }

    if (!paused) {
      const int brush_phase = frame % PHASE_COUNT;
      const int64_t control = encode_control(mouse_x, mouse_y, brush_phase, mouse_down, steps_per_frame);
      hyd_array_t* next = hyd_export_excitable_frame(state, control);
      hyd_array_free(state);
      state = next;
      frame += steps_per_frame;
    }

    const int64_t* phase = hyd_array_data_int64(state);
    for (int i = 0; i < GRID_H; ++i) {
      for (int j = 0; j < GRID_W; ++j) {
        pixels[i * GRID_W + j] = toARGB(colorize_phase(phase[i * GRID_W + j]), pf);
      }
    }

    SDL_UpdateTexture(tex, nullptr, pixels.data(), GRID_W * static_cast<int>(sizeof(Uint32)));
    SDL_RenderClear(renderer);
    SDL_Rect dst{0, 0, GRID_W * scale, GRID_H * scale};
    SDL_RenderCopy(renderer, tex, nullptr, &dst);
    SDL_RenderPresent(renderer);

    char title[160];
    std::snprintf(
      title, sizeof(title),
      "Excitable Media [Hydrangea]  step=%d  spf=%d  scale=%dx%s",
      frame, steps_per_frame, scale, paused ? "  [PAUSED]" : "");
    SDL_SetWindowTitle(window, title);
  }

  hyd_array_free(state);
  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
