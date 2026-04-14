#include <SDL.h>

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <unistd.h>
#include <vector>

#include "point_cloud_live.h"

static constexpr int RENDER_W = 768;
static constexpr int RENDER_H = 768;
static constexpr int WINDOW_SCALE = 1;

enum ResolveMode {
  RESOLVE_MIN = 0,
  RESOLVE_ADD = 1,
  RESOLVE_MAX = 2,
};

static const char* mode_name(ResolveMode mode) {
  switch (mode) {
    case RESOLVE_MIN:
      return "min";
    case RESOLVE_ADD:
      return "+";
    case RESOLVE_MAX:
      return "max";
  }
  return "?";
}

int main(int /*argc*/, char** /*argv*/) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    SDL_Log("SDL_Init error: %s", SDL_GetError());
    return 1;
  }

  {
    const char* base = SDL_GetBasePath();
    if (!base) {
      SDL_Log("SDL_GetBasePath error: %s", SDL_GetError());
      SDL_Quit();
      return 1;
    }
    if (chdir(base) != 0) {
      SDL_Log("chdir(%s) error", base);
      SDL_free(const_cast<char*>(base));
      SDL_Quit();
      return 1;
    }
    SDL_free(const_cast<char*>(base));
  }

  SDL_Window* window = SDL_CreateWindow(
    "Stanford Bunny [Hydrangea]",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
    RENDER_W * WINDOW_SCALE, RENDER_H * WINDOW_SCALE,
    SDL_WINDOW_RESIZABLE
  );
  if (!window) {
    SDL_Log("SDL_CreateWindow error: %s", SDL_GetError());
    SDL_Quit();
    return 1;
  }

  SDL_Renderer* renderer = SDL_CreateRenderer(
    window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC
  );
  if (!renderer) {
    SDL_Log("SDL_CreateRenderer error: %s", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }
  SDL_RenderSetLogicalSize(renderer, RENDER_W, RENDER_H);

  SDL_Texture* tex = SDL_CreateTexture(
    renderer,
    SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
    RENDER_W, RENDER_H
  );
  if (!tex) {
    SDL_Log("SDL_CreateTexture error: %s", SDL_GetError());
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }

  SDL_PixelFormat* pf = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);
  std::vector<Uint32> pixels(RENDER_W * RENDER_H);

  double yaw = 0.45;
  double pitch = -0.30;
  bool mouse_down = false;
  ResolveMode mode = RESOLVE_MIN;

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
            case SDLK_r:
              yaw = 0.45;
              pitch = -0.30;
              mode = RESOLVE_MIN;
              break;
            case SDLK_1:
              mode = RESOLVE_MIN;
              break;
            case SDLK_2:
              mode = RESOLVE_ADD;
              break;
            case SDLK_3:
              mode = RESOLVE_MAX;
              break;
            case SDLK_TAB:
            case SDLK_m:
              mode = static_cast<ResolveMode>((static_cast<int>(mode) + 1) % 3);
              break;
          }
          break;

        case SDL_MOUSEBUTTONDOWN:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = true;
          }
          break;

        case SDL_MOUSEBUTTONUP:
          if (ev.button.button == SDL_BUTTON_LEFT) {
            mouse_down = false;
          }
          break;

        case SDL_MOUSEMOTION:
          if (mouse_down) {
            yaw += static_cast<double>(ev.motion.xrel) * 0.010;
            pitch += static_cast<double>(ev.motion.yrel) * 0.010;
            pitch = std::clamp(pitch, -1.45, 1.45);
          }
          break;
      }
    }

    const int64_t yaw_milli = static_cast<int64_t>(yaw * 1000.0);
    const int64_t pitch_milli = static_cast<int64_t>(pitch * 1000.0);
    hyd_array_t* rgb = hyd_export_point_cloud_frame(
      yaw_milli, pitch_milli, static_cast<int64_t>(mode)
    );
    const int64_t* packed = hyd_array_data_int64(rgb);

    for (int i = 0; i < RENDER_W * RENDER_H; ++i) {
      const uint32_t v = static_cast<uint32_t>(packed[i]);
      const Uint8 r = static_cast<Uint8>((v >> 16) & 255u);
      const Uint8 g = static_cast<Uint8>((v >> 8) & 255u);
      const Uint8 b = static_cast<Uint8>(v & 255u);
      pixels[i] = SDL_MapRGB(pf, r, g, b);
    }

    hyd_array_free(rgb);

    SDL_UpdateTexture(tex, nullptr, pixels.data(), RENDER_W * static_cast<int>(sizeof(Uint32)));
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, tex, nullptr, nullptr);
    SDL_RenderPresent(renderer);

    char title[160];
    std::snprintf(
      title, sizeof(title),
      "Stanford Bunny [Hydrangea]  mode=%s  yaw=%.2f  pitch=%.2f",
      mode_name(mode), yaw, pitch
    );
    SDL_SetWindowTitle(window, title);
  }

  SDL_FreeFormat(pf);
  SDL_DestroyTexture(tex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}
