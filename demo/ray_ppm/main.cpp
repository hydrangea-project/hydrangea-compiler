#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#ifdef USE_METAL
#include <SDL.h>
#endif

// The correct header is force-included via -include in the build scripts.
// Fall back to default names if force-include is not used.
#if !defined(HYDRANGEA_EXPORT_RAY_PPM_H) && !defined(HYDRANGEA_METAL_EXPORT_RAY_PPM_H)
  #ifdef USE_METAL
  #include "ray_ppm_metal.h"
  #else
  #include "ray_ppm.h"
  #endif
#endif

namespace {

struct RGB {
  unsigned char r;
  unsigned char g;
  unsigned char b;
};

struct Vec3 {
  double x;
  double y;
  double z;
};

Vec3 operator+(Vec3 a, Vec3 b) {
  return Vec3 {a.x + b.x, a.y + b.y, a.z + b.z};
}

Vec3 operator*(double s, Vec3 v) {
  return Vec3 {s * v.x, s * v.y, s * v.z};
}

Vec3 lerp(Vec3 a, Vec3 b, double t) {
  return (1.0 - t) * a + t * b;
}

double clamp01(double x) {
  return std::clamp(x, 0.0, 1.0);
}

double smoothstep(double edge0, double edge1, double x) {
  const double t = clamp01((x - edge0) / std::max(1.0e-9, edge1 - edge0));
  return t * t * (3.0 - 2.0 * t);
}

RGB toRGB(Vec3 c) {
  return RGB
    { static_cast<unsigned char>(std::clamp(255.0 * c.x, 0.0, 255.0))
    , static_cast<unsigned char>(std::clamp(255.0 * c.y, 0.0, 255.0))
    , static_cast<unsigned char>(std::clamp(255.0 * c.z, 0.0, 255.0))
    };
}

Vec3 imageBackground(double v) {
  const Vec3 horizon {0.035, 0.038, 0.05};
  const Vec3 zenith {0.01, 0.015, 0.03};
  return lerp(horizon, zenith, clamp01(v));
}

Vec3 gradeLuminance(double t) {
  t = clamp01(t);
  const Vec3 shadow {0.06, 0.08, 0.12};
  const Vec3 mid {0.47, 0.54, 0.68};
  const Vec3 hot {1.0, 0.9, 0.75};
  if (t < 0.58) {
    return lerp(shadow, mid, smoothstep(0.0, 0.58, t));
  }
  return lerp(mid, hot, smoothstep(0.58, 1.0, t));
}

void writePPM(const std::string& path, int width, int height, const std::vector<RGB>& pixels) {
  std::ofstream out(path, std::ios::binary);
  if (!out) {
    throw std::runtime_error("failed to open output file: " + path);
  }
  out << "P6\n" << width << " " << height << "\n255\n";
  out.write(reinterpret_cast<const char*>(pixels.data()),
            static_cast<std::streamsize>(pixels.size() * sizeof(RGB)));
}

std::string executableDir(const char* argv0) {
  if (argv0 == nullptr) {
    return {};
  }
  const std::string path(argv0);
  const std::size_t slash = path.find_last_of('/');
  if (slash == std::string::npos) {
    return {};
  }
  return path.substr(0, slash + 1);
}

}  // namespace

int main(int argc, char** argv) {
  const std::string outputPath = argc > 1 ? argv[1] : "ray_still_life.ppm";

  #ifdef USE_METAL
  #ifndef METALLIB_NAME
  #define METALLIB_NAME "ray_ppm.metallib"
  #endif
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    std::cerr << "SDL_Init error: " << SDL_GetError() << "\n";
    return 1;
  }
  SDL_Window* bootstrapWindow = SDL_CreateWindow(
      "Ray PPM Metal Bootstrap", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1, 1,
      SDL_WINDOW_HIDDEN);
  if (bootstrapWindow == nullptr) {
    std::cerr << "SDL_CreateWindow error: " << SDL_GetError() << "\n";
    SDL_Quit();
    return 1;
  }
  const std::string metallibPath = executableDir(argc > 0 ? argv[0] : nullptr) + METALLIB_NAME;
  if (hyd_metal_init(metallibPath.c_str()) != 0) {
    std::cerr << "hyd_metal_init failed for " << metallibPath << "\n";
    SDL_DestroyWindow(bootstrapWindow);
    SDL_Quit();
    return 1;
  }
  hyd_array_t* render = hyd_metal_ray_ppm();
  #else
  hyd_array_t* render = hyd_export_ray_ppm();
  #endif
  if (render == nullptr) {
    #ifdef USE_METAL
    hyd_metal_cleanup();
    SDL_DestroyWindow(bootstrapWindow);
    SDL_Quit();
    #endif
    std::cerr << "hyd_export_ray_ppm returned null\n";
    return 1;
  }

  if (hyd_array_ndims(render) != 2) {
    std::cerr << "expected a 2D render array [H,W], got " << hyd_array_ndims(render)
              << " dimensions\n";
    hyd_array_free(render);
    #ifdef USE_METAL
    hyd_metal_cleanup();
    SDL_DestroyWindow(bootstrapWindow);
    SDL_Quit();
    #endif
    return 1;
  }

  const int outHeight = static_cast<int>(hyd_array_dim(render, 0));
  const int outWidth = static_cast<int>(hyd_array_dim(render, 1));
  const double* luminance = hyd_array_data_f64(render);

  double maxLuminance = 0.0;
  double meanLuminance = 0.0;
  #pragma omp parallel for reduction(max:maxLuminance) reduction(+:meanLuminance) schedule(static)
  for (int i = 0; i < outHeight * outWidth; ++i) {
    maxLuminance = std::max(maxLuminance, luminance[i]);
    meanLuminance += luminance[i];
  }
  meanLuminance /= std::max(1, outHeight * outWidth);

  const double exposure = 2.8 / std::max(1.0e-6, std::max(maxLuminance, meanLuminance * 6.0));
  std::vector<RGB> pixels(static_cast<std::size_t>(outWidth * outHeight));

  #pragma omp parallel for schedule(static)
  for (int y = 0; y < outHeight; ++y) {
    for (int x = 0; x < outWidth; ++x) {
      const std::size_t idx = static_cast<std::size_t>(y * outWidth + x);
      const double mapped = 1.0 - std::exp(-exposure * luminance[idx]);
      const double graded = std::pow(clamp01(mapped), 0.9);
      const double blend = smoothstep(0.015, 0.12, graded);
      const double v = 1.0 - static_cast<double>(y) / std::max(1, outHeight - 1);
      const double nx = (2.0 * (static_cast<double>(x) + 0.5) / outWidth) - 1.0;
      const double ny = (2.0 * (static_cast<double>(y) + 0.5) / outHeight) - 1.0;
      const double vignette = 1.0 - 0.16 * std::clamp(nx * nx + 0.8 * ny * ny, 0.0, 1.0);
      const Vec3 bg = vignette * imageBackground(v);
      const Vec3 fg = gradeLuminance(graded);
      pixels[idx] = toRGB(lerp(bg, fg, blend));
    }
  }

  try {
    writePPM(outputPath, outWidth, outHeight, pixels);
  } catch (const std::exception& ex) {
    std::cerr << ex.what() << "\n";
    hyd_array_free(render);
    #ifdef USE_METAL
    hyd_metal_cleanup();
    SDL_DestroyWindow(bootstrapWindow);
    SDL_Quit();
    #endif
    return 1;
  }

  hyd_array_free(render);
  #ifdef USE_METAL
  hyd_metal_cleanup();
  SDL_DestroyWindow(bootstrapWindow);
  SDL_Quit();
  #endif
  std::cout << "Wrote " << outputPath << " (" << outWidth << "x" << outHeight << ")\n";
  return 0;
}
