#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "gray_scott.h"

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

// Map raw V concentration (nominally [0, 0.5]) to a 3-stop colormap:
//   V = 0   → deep navy   (background, only U present)
//   V = 0.25 → teal
//   V = 0.5  → bright yellow (active spot)
Vec3 colorize_gs(double v) {
  const double t = clamp01(v / 0.5);
  const Vec3 dark   {0.02, 0.04, 0.12};
  const Vec3 mid    {0.05, 0.55, 0.65};
  const Vec3 bright {0.98, 0.92, 0.20};
  if (t < 0.5)
    return lerp(dark, mid, smoothstep(0.0, 0.5, t));
  else
    return lerp(mid, bright, smoothstep(0.5, 1.0, t));
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
  const std::string outputPath = argc > 1 ? argv[1] : "gs_demo.ppm";

  hyd_array_t* result = hyd_export_gray_scott();
  if (result == nullptr) {
    std::cerr << "hyd_export_gray_scott returned null\n";
    return 1;
  }

  if (hyd_array_ndims(result) != 2) {
    std::cerr << "expected a 2D array [H,W], got " << hyd_array_ndims(result) << " dimensions\n";
    hyd_array_free(result);
    return 1;
  }

  const int height = static_cast<int>(hyd_array_dim(result, 0));
  const int width  = static_cast<int>(hyd_array_dim(result, 1));
  const double* v_data = hyd_array_data_f64(result);

  std::vector<RGB> pixels(static_cast<std::size_t>(width * height));

  #pragma omp parallel for schedule(static)
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      const std::size_t idx = static_cast<std::size_t>(y * width + x);
      pixels[idx] = toRGB(colorize_gs(v_data[idx]));
    }
  }

  try {
    writePPM(outputPath, width, height, pixels);
  } catch (const std::exception& ex) {
    std::cerr << ex.what() << "\n";
    hyd_array_free(result);
    return 1;
  }

  hyd_array_free(result);
  std::cout << "Wrote " << outputPath << " (" << width << "x" << height << ")\n";
  return 0;
}
