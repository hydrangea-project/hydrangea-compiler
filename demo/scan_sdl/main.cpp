#include <SDL.h>

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <vector>

#include "scan_live.h"

static constexpr int GRID_H = 256;
static constexpr int GRID_W = 256;

static int clamp_i(int x, int lo, int hi) {
  return std::max(lo, std::min(x, hi));
}

int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  int scale = 2;
  int steps_per_frame = 2;
  bool paused = false;
  bool mouse_down = false;
  int frame = 0;
  int mouse_x = GRID_W / 2;
  int mouse_y = GRID_H / 2;

  SDL_Window* window = SDL_CreateWindow(
    "Scan Light Sweep [Hydrangea]",
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
  std::vector<Uint32> pixels(GRID_W * GRID_H);

  bool running = true;
  SDL_Event ev;

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
      frame += steps_per_frame;
    }

    const int64_t mouse_encoded = static_cast<int64_t>(mouse_x) * 10000 + mouse_y;
    hyd_array_t* rgb = hyd_export_scan_frame(frame, mouse_encoded);
    const int64_t* packed = hyd_array_data_int64(rgb);

    for (int i = 0; i < GRID_H * GRID_W; ++i) {
      const uint32_t v = static_cast<uint32_t>(packed[i]);
      const Uint8 r = static_cast<Uint8>((v >> 16) & 255u);
      const Uint8 g = static_cast<Uint8>((v >> 8) & 255u);
      const Uint8 b = static_cast<Uint8>(v & 255u);
      pixels[i] = SDL_MapRGB(pf, r, g, b);
    }

    hyd_array_free(rgb);

    SDL_UpdateTexture(tex, nullptr, pixels.data(), GRID_W * static_cast<int>(sizeof(Uint32)));
    SDL_RenderClear(renderer);
    SDL_Rect dst{0, 0, GRID_W * scale, GRID_H * scale};
    SDL_RenderCopy(renderer, tex, nullptr, &dst);
    SDL_RenderPresent(renderer);

    char title[160];
    std::snprintf(title, sizeof(title),
      "Scan Light Sweep [Hydrangea]  frame=%d  spf=%d  scale=%dx%s",
      frame, steps_per_frame, scale, paused ? "  [PAUSED]" : "");
    SDL_SetWindowTitle(window, title);
  }

  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}

