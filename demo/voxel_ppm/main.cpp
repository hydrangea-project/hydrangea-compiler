#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "render.h"

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

Vec3 colorize(double t) {
  t = clamp01(t);
  const double shadow = smoothstep(0.0, 0.45, t);
  const double highlight = smoothstep(0.45, 1.0, t);
  const Vec3 deep {0.05, 0.08, 0.16};
  const Vec3 mid {0.9, 0.56, 0.2};
  const Vec3 hot {1.0, 0.93, 0.8};
  return lerp(lerp(deep, mid, shadow), hot, 0.7 * highlight);
}

Vec3 backgroundColor(double v) {
  const Vec3 zenith {0.02, 0.03, 0.07};
  const Vec3 horizon {0.12, 0.08, 0.1};
  return lerp(horizon, zenith, clamp01(v));
}

void writePPM(const std::string& path, int width, int height, const std::vector<RGB>& pixels) {
  std::ofstream out(path, std::ios::binary);
  if (!out) {
    throw std::runtime_error("failed to open output file: " + path);
  }
  out << "P6\n" << width << " " << height << "\n255\n";
  out.write(reinterpret_cast<const char*>(pixels.data()), static_cast<std::streamsize>(pixels.size() * sizeof(RGB)));
}

}  // namespace

int main(int argc, char** argv) {
  const std::string outputPath = argc > 1 ? argv[1] : "voxel_demo.ppm";

  hyd_array_t* render = hyd_export_render();
  if (render == nullptr) {
    std::cerr << "hyd_export_render returned null\n";
    return 1;
  }

  if (hyd_array_ndims(render) != 2) {
    std::cerr << "expected a 2D render array [H,W], got " << hyd_array_ndims(render) << " dimensions\n";
    hyd_array_free(render);
    return 1;
  }

  const int outHeight = static_cast<int>(hyd_array_dim(render, 0));
  const int outWidth  = static_cast<int>(hyd_array_dim(render, 1));
  const double* luminance = hyd_array_data_f64(render);

  double maxLuminance = 0.0;
  #pragma omp parallel for reduction(max:maxLuminance) schedule(static)
  for (int i = 0; i < outHeight * outWidth; ++i) {
    maxLuminance = std::max(maxLuminance, luminance[i]);
  }

  std::vector<RGB> pixels(static_cast<std::size_t>(outWidth * outHeight));

  #pragma omp parallel for schedule(static)
  for (int y = 0; y < outHeight; ++y) {
    for (int x = 0; x < outWidth; ++x) {
      const std::size_t dstIdx = static_cast<std::size_t>(y * outWidth + x);
      const double normalized = maxLuminance > 0.0 ? luminance[dstIdx] / maxLuminance : 0.0;
      const double v = 1.0 - static_cast<double>(y) / std::max(1, outHeight - 1);
      const double nxv = (2.0 * (static_cast<double>(x) + 0.5) / outWidth) - 1.0;
      const double nyv = (2.0 * (static_cast<double>(y) + 0.5) / outHeight) - 1.0;
      const double vignette = 1.0 - 0.18 * std::clamp(nxv * nxv + 0.7 * nyv * nyv, 0.0, 1.0);
      const Vec3 bg = vignette * backgroundColor(v);
      const Vec3 fg = colorize(std::pow(normalized, 0.82));
      const double blend = smoothstep(0.02, 0.16, normalized);
      pixels[dstIdx] = toRGB(lerp(bg, fg, blend));
    }
  }

  try {
    writePPM(outputPath, outWidth, outHeight, pixels);
  } catch (const std::exception& ex) {
    std::cerr << ex.what() << "\n";
    hyd_array_free(render);
    return 1;
  }

  hyd_array_free(render);
  std::cout << "Wrote " << outputPath << " (" << outWidth << "x" << outHeight << ")\n";
  return 0;
}
