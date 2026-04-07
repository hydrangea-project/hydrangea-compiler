#include <SDL.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <vector>

#include "pic_density.h"

// ---------------------------------------------------------------------------
// Dimensions — must match pic_density.hyd constants
// ---------------------------------------------------------------------------
static constexpr int NUM_PARTICLES = 400;
static constexpr int STATE_SIZE    = NUM_PARTICLES * 4;  // x, y, vx, vy per particle
static constexpr int GRID_H        = 128;
static constexpr int GRID_W        = 128;
static constexpr int NUM_CELLS     = GRID_H * GRID_W;

// Display
static constexpr int SCALE = 4;                        // grid cell -> pixels
static constexpr int WIN_W = GRID_W * SCALE;           // 512
static constexpr int WIN_H = GRID_H * SCALE;           // 512

// ---------------------------------------------------------------------------
// Smoothstep for colour interpolation
// ---------------------------------------------------------------------------
static inline double smoothstep(double e0, double e1, double x) {
  double t = std::clamp((x - e0) / (e1 - e0), 0.0, 1.0);
  return t * t * (3.0 - 2.0 * t);
}

// ---------------------------------------------------------------------------
// Density heat map: dark blue -> teal -> yellow -> white
// ---------------------------------------------------------------------------
static void density_color(double v, double vmax, Uint8& r, Uint8& g, Uint8& b) {
  double t = (vmax > 0.0) ? std::clamp(v / vmax, 0.0, 1.0) : 0.0;
  // Apply sqrt for perceptual scaling (makes low-density regions more visible)
  t = std::sqrt(t);
  if (t < 0.33) {
    double s = smoothstep(0.0, 0.33, t);
    r = static_cast<Uint8>(8 + 20 * s);
    g = static_cast<Uint8>(12 + 80 * s);
    b = static_cast<Uint8>(40 + 100 * s);
  } else if (t < 0.66) {
    double s = smoothstep(0.33, 0.66, t);
    r = static_cast<Uint8>(28 + 200 * s);
    g = static_cast<Uint8>(92 + 130 * s);
    b = static_cast<Uint8>(140 - 80 * s);
  } else {
    double s = smoothstep(0.66, 1.0, t);
    r = static_cast<Uint8>(228 + 27 * s);
    g = static_cast<Uint8>(222 + 33 * s);
    b = static_cast<Uint8>(60 + 195 * s);
  }
}

// ---------------------------------------------------------------------------
// Create initial particle state: ring around grid centre with tangential velocity
// ---------------------------------------------------------------------------
static hyd_array_t* make_initial_state() {
  hyd_tuple_t shape = hyd_tuple_make(1, static_cast<int64_t>(STATE_SIZE));
  hyd_array_t* state = hyd_array_alloc_bytes(shape, sizeof(double));
  double* d = hyd_array_data_f64(state);

  const double cx = GRID_W * 0.5;
  const double cy = GRID_H * 0.5;
  const double radius = GRID_W * 0.3;
  const double speed  = 12.0;  // tangential speed

  for (int i = 0; i < NUM_PARTICLES; ++i) {
    // Spread particles evenly around a ring with some radial jitter
    double angle = 2.0 * M_PI * i / NUM_PARTICLES;
    // Deterministic jitter from particle index
    double jr = std::sin(i * 7.3 + 1.1) * 0.35 * radius;
    double r  = radius + jr;

    double px = cx + r * std::cos(angle);
    double py = cy + r * std::sin(angle);

    // Tangential velocity (perpendicular to radius, counter-clockwise)
    double vx = -speed * std::sin(angle) * (0.8 + 0.4 * std::sin(i * 3.7));
    double vy =  speed * std::cos(angle) * (0.8 + 0.4 * std::sin(i * 3.7));

    d[i * 4 + 0] = px;
    d[i * 4 + 1] = py;
    d[i * 4 + 2] = vx;
    d[i * 4 + 3] = vy;
  }
  return state;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  SDL_Window* window = SDL_CreateWindow(
    "Particle-in-Cell Density [Hydrangea]",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    WIN_W, WIN_H, 0);
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

  SDL_Texture* tex = SDL_CreateTexture(renderer,
    SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
    WIN_W, WIN_H);
  SDL_PixelFormat* pf = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);
  std::vector<Uint32> pixels(WIN_W * WIN_H);

  // Particle state
  hyd_array_t* state = make_initial_state();

  // Attractor position (grid coordinates)
  double attr_gx = GRID_W * 0.5;
  double attr_gy = GRID_H * 0.5;
  bool mouse_down = false;
  bool paused = false;
  int steps_per_frame = 3;
  int frame = 0;

  bool running = true;
  SDL_Event ev;

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
            case SDLK_SPACE:
              paused = !paused;
              break;
            case SDLK_r: {
              hyd_array_free(state);
              state = make_initial_state();
              attr_gx = GRID_W * 0.5;
              attr_gy = GRID_H * 0.5;
              break;
            }
            case SDLK_EQUALS:
            case SDLK_PLUS:
              steps_per_frame = std::min(20, steps_per_frame + 1);
              break;
            case SDLK_MINUS:
              steps_per_frame = std::max(1, steps_per_frame - 1);
              break;
          }
          break;

        case SDL_MOUSEBUTTONDOWN:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = true;
            attr_gx = static_cast<double>(ev.button.x) / SCALE;
            attr_gy = static_cast<double>(ev.button.y) / SCALE;
          }
          break;

        case SDL_MOUSEBUTTONUP:
          if (ev.button.button == SDL_BUTTON_LEFT)
            mouse_down = false;
          break;

        case SDL_MOUSEMOTION:
          if (mouse_down) {
            attr_gx = static_cast<double>(ev.motion.x) / SCALE;
            attr_gy = static_cast<double>(ev.motion.y) / SCALE;
          }
          break;
      }
    }

    // --- Simulation steps ---
    hyd_array_t* density_arr = nullptr;
    if (!paused) {
      for (int s = 0; s < steps_per_frame; ++s) {
        // Encode attractor position: ax * 10000 + ay
        // Clamp to [0, grid-1] range for encoding (attractor force works at any position)
        int64_t ax_i = static_cast<int64_t>(std::clamp(attr_gx, 0.0, static_cast<double>(GRID_W - 1)));
        int64_t ay_i = static_cast<int64_t>(std::clamp(attr_gy, 0.0, static_cast<double>(GRID_H - 1)));
        int64_t attractor_encoded = ax_i * 10000 + ay_i;

        hyd_pair_aa_t result = hyd_export_pic_step(state, attractor_encoded);

        hyd_array_free(state);
        state = result.fst;

        // Keep density from last sub-step only
        if (density_arr) hyd_array_free(density_arr);
        density_arr = result.snd;
      }
      ++frame;
    }

    // If paused and no density yet, run one step just to get a density snapshot
    if (!density_arr) {
      int64_t ax_i = static_cast<int64_t>(std::clamp(attr_gx, 0.0, static_cast<double>(GRID_W - 1)));
      int64_t ay_i = static_cast<int64_t>(std::clamp(attr_gy, 0.0, static_cast<double>(GRID_H - 1)));
      int64_t attractor_encoded = ax_i * 10000 + ay_i;
      hyd_pair_aa_t result = hyd_export_pic_step(state, attractor_encoded);
      // Don't update state when paused — discard new_state, keep density
      hyd_array_free(result.fst);
      density_arr = result.snd;
    }

    const double* density = hyd_array_data_f64(density_arr);

    // --- Find density max for normalisation ---
    double dmax = 0.0;
    for (int i = 0; i < NUM_CELLS; ++i) {
      if (density[i] > dmax) dmax = density[i];
    }
    // Use at least 1.0 to avoid division by zero; clamp high end so
    // single-particle cells still register colour
    dmax = std::max(dmax, 1.0);

    // --- Render density grid ---
    for (int gy = 0; gy < GRID_H; ++gy) {
      for (int gx = 0; gx < GRID_W; ++gx) {
        double val = density[gy * GRID_W + gx];
        Uint8 cr, cg, cb;
        density_color(val, dmax, cr, cg, cb);
        Uint32 col = SDL_MapRGB(pf, cr, cg, cb);

        // Fill the SCALE x SCALE block
        for (int dy = 0; dy < SCALE; ++dy) {
          for (int dx = 0; dx < SCALE; ++dx) {
            pixels[(gy * SCALE + dy) * WIN_W + gx * SCALE + dx] = col;
          }
        }
      }
    }

    // --- Overlay particle dots (small bright circles) ---
    const double* st = hyd_array_data_f64(state);
    Uint32 dot_color = SDL_MapRGB(pf, 255, 255, 255);
    Uint32 dot_outline = SDL_MapRGB(pf, 40, 40, 40);

    for (int i = 0; i < NUM_PARTICLES; ++i) {
      int sx = static_cast<int>(st[i * 4 + 0] * SCALE);
      int sy = static_cast<int>(st[i * 4 + 1] * SCALE);

      // Draw a 3x3 dot with dark outline
      for (int dy = -2; dy <= 2; ++dy) {
        for (int dx = -2; dx <= 2; ++dx) {
          int px = sx + dx;
          int py = sy + dy;
          if (px < 0 || px >= WIN_W || py < 0 || py >= WIN_H) continue;
          int d2 = dx * dx + dy * dy;
          if (d2 <= 2) {
            pixels[py * WIN_W + px] = dot_color;
          } else if (d2 <= 5) {
            pixels[py * WIN_W + px] = dot_outline;
          }
        }
      }
    }

    // --- Draw attractor crosshair ---
    {
      int ax = static_cast<int>(attr_gx * SCALE);
      int ay = static_cast<int>(attr_gy * SCALE);
      Uint32 cross_color = SDL_MapRGB(pf, 255, 80, 80);
      for (int d = -8; d <= 8; ++d) {
        int px, py;
        // Horizontal arm
        px = ax + d; py = ay;
        if (px >= 0 && px < WIN_W && py >= 0 && py < WIN_H)
          pixels[py * WIN_W + px] = cross_color;
        // Vertical arm
        px = ax; py = ay + d;
        if (px >= 0 && px < WIN_W && py >= 0 && py < WIN_H)
          pixels[py * WIN_W + px] = cross_color;
      }
    }

    // --- Present ---
    SDL_UpdateTexture(tex, nullptr, pixels.data(),
                      WIN_W * static_cast<int>(sizeof(Uint32)));
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, tex, nullptr, nullptr);
    SDL_RenderPresent(renderer);

    hyd_array_free(density_arr);

    // --- Update title ---
    char title[128];
    std::snprintf(title, sizeof(title),
      "PIC Density [Hydrangea]  frame=%d  steps/frame=%d%s",
      frame, steps_per_frame, paused ? "  PAUSED" : "");
    SDL_SetWindowTitle(window, title);
  }

  // Cleanup
  hyd_array_free(state);
  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
