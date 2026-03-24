#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "voxel_scene.h"

namespace {

struct RGB {
  unsigned char r;
  unsigned char g;
  unsigned char b;
};

struct ImageSize {
  int width;
  int height;
};

struct Vec3 {
  double x;
  double y;
  double z;
};

Vec3 operator+(Vec3 a, Vec3 b) {
  return Vec3 {a.x + b.x, a.y + b.y, a.z + b.z};
}

Vec3 operator-(Vec3 a, Vec3 b) {
  return Vec3 {a.x - b.x, a.y - b.y, a.z - b.z};
}

Vec3 operator*(double s, Vec3 v) {
  return Vec3 {s * v.x, s * v.y, s * v.z};
}

Vec3 operator*(Vec3 v, double s) {
  return s * v;
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

int parsePositiveInt(const std::string& text, const std::string& what) {
  char* end = nullptr;
  const long value = std::strtol(text.c_str(), &end, 10);
  if (end == text.c_str() || *end != '\0' || value <= 0) {
    throw std::runtime_error("invalid " + what + ": " + text);
  }
  return static_cast<int>(value);
}

ImageSize parseImageSizeArg(const std::string& text) {
  const std::size_t sep = text.find('x');
  if (sep == std::string::npos) {
    const int side = parsePositiveInt(text, "image size");
    return ImageSize {side, side};
  }
  const int width = parsePositiveInt(text.substr(0, sep), "image width");
  const int height = parsePositiveInt(text.substr(sep + 1), "image height");
  return ImageSize {width, height};
}

double length(Vec3 v) {
  return std::sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
}

Vec3 normalize(Vec3 v) {
  const double len = length(v);
  if (len <= 1.0e-9) {
    return Vec3 {0.0, 0.0, 1.0};
  }
  return Vec3 {v.x / len, v.y / len, v.z / len};
}

double dot(Vec3 a, Vec3 b) {
  return (a.x * b.x) + (a.y * b.y) + (a.z * b.z);
}

Vec3 cross(Vec3 a, Vec3 b) {
  return Vec3
    { a.y * b.z - a.z * b.y
    , a.z * b.x - a.x * b.z
    , a.x * b.y - a.y * b.x
    };
}

double pixelJitter(int x, int y) {
  uint32_t state = static_cast<uint32_t>(x) * 0x9E3779B9u ^ static_cast<uint32_t>(y) * 0x85EBCA6Bu;
  state ^= state >> 16;
  state *= 0x7FEB352Du;
  state ^= state >> 15;
  state *= 0x846CA68Bu;
  state ^= state >> 16;
  return (state & 0x00FFFFFFu) / static_cast<double>(0x01000000u);
}

bool intersectBox(Vec3 origin, Vec3 dir, Vec3 bmin, Vec3 bmax, double& tEnter, double& tExit) {
  tEnter = 0.0;
  tExit = 1.0e30;
  const auto updateSlab = [&](double o, double d, double lo, double hi) -> bool {
    if (std::abs(d) < 1.0e-9) {
      return o >= lo && o <= hi;
    }
    double t0 = (lo - o) / d;
    double t1 = (hi - o) / d;
    if (t0 > t1) {
      std::swap(t0, t1);
    }
    tEnter = std::max(tEnter, t0);
    tExit = std::min(tExit, t1);
    return tEnter <= tExit;
  };
  return
    updateSlab(origin.x, dir.x, bmin.x, bmax.x) &&
    updateSlab(origin.y, dir.y, bmin.y, bmax.y) &&
    updateSlab(origin.z, dir.z, bmin.z, bmax.z);
}

Vec3 sampleGradient(
  const double* data,
  int nx,
  int ny,
  int nz,
  int x,
  int y,
  int z
) {
  const auto clampIndex = [](int value, int upper) {
    return std::clamp(value, 0, upper - 1);
  };
  const auto at = [&](int sx, int sy, int sz) -> double {
    sx = clampIndex(sx, nx);
    sy = clampIndex(sy, ny);
    sz = clampIndex(sz, nz);
    const std::size_t index = static_cast<std::size_t>(((sz * ny) + sy) * nx + sx);
    return static_cast<double>(data[index]);
  };
  return Vec3
    { at(x + 1, y, z) - at(x - 1, y, z)
    , at(x, y + 1, z) - at(x, y - 1, z)
    , at(x, y, z + 1) - at(x, y, z - 1)
    };
}

double sampleTrilinear(
  const double* data,
  int nx,
  int ny,
  int nz,
  double x,
  double y,
  double z
) {
  const auto clampCoord = [](double value, int upper) {
    return std::clamp(value, 0.0, static_cast<double>(upper - 1));
  };
  const auto at = [&](int sx, int sy, int sz) -> double {
    sx = std::clamp(sx, 0, nx - 1);
    sy = std::clamp(sy, 0, ny - 1);
    sz = std::clamp(sz, 0, nz - 1);
    const std::size_t index = static_cast<std::size_t>(((sz * ny) + sy) * nx + sx);
    return static_cast<double>(data[index]);
  };

  x = clampCoord(x, nx);
  y = clampCoord(y, ny);
  z = clampCoord(z, nz);

  const int x0 = static_cast<int>(std::floor(x));
  const int y0 = static_cast<int>(std::floor(y));
  const int z0 = static_cast<int>(std::floor(z));
  const int x1 = std::min(x0 + 1, nx - 1);
  const int y1 = std::min(y0 + 1, ny - 1);
  const int z1 = std::min(z0 + 1, nz - 1);
  const double tx = x - x0;
  const double ty = y - y0;
  const double tz = z - z0;

  const double c000 = at(x0, y0, z0);
  const double c100 = at(x1, y0, z0);
  const double c010 = at(x0, y1, z0);
  const double c110 = at(x1, y1, z0);
  const double c001 = at(x0, y0, z1);
  const double c101 = at(x1, y0, z1);
  const double c011 = at(x0, y1, z1);
  const double c111 = at(x1, y1, z1);

  const double c00 = c000 * (1.0 - tx) + c100 * tx;
  const double c10 = c010 * (1.0 - tx) + c110 * tx;
  const double c01 = c001 * (1.0 - tx) + c101 * tx;
  const double c11 = c011 * (1.0 - tx) + c111 * tx;
  const double c0 = c00 * (1.0 - ty) + c10 * ty;
  const double c1 = c01 * (1.0 - ty) + c11 * ty;
  return c0 * (1.0 - tz) + c1 * tz;
}

double sampleShadow(
  const double* data,
  int nx,
  int ny,
  int nz,
  Vec3 start,
  Vec3 lightDir
) {
  const double shadowStep = 1.5;
  double occlusion = 0.0;
  for (int stepIndex = 0; stepIndex < 14 && occlusion < 1.0; ++stepIndex) {
    const double dist = (stepIndex + 1) * shadowStep;
    const Vec3 p = start + dist * lightDir;
    if (p.x < 0.0 || p.x > nx - 1 || p.y < 0.0 || p.y > ny - 1 || p.z < 0.0 || p.z > nz - 1) {
      break;
    }
    const double density = sampleTrilinear(data, nx, ny, nz, p.x, p.y, p.z);
    occlusion += std::clamp(density / 60.0, 0.0, 0.12);
  }
  return std::clamp(1.0 - occlusion, 0.18, 1.0);
}

double refineSurfaceHit(
  const double* data,
  int nx,
  int ny,
  int nz,
  Vec3 origin,
  Vec3 dir,
  double tLo,
  double tHi,
  double threshold
) {
  double lo = tLo;
  double hi = tHi;
  for (int iter = 0; iter < 5; ++iter) {
    const double mid = 0.5 * (lo + hi);
    const Vec3 p = origin + mid * dir;
    const double d = sampleTrilinear(data, nx, ny, nz, p.x, p.y, p.z);
    if (d >= threshold) {
      hi = mid;
    } else {
      lo = mid;
    }
  }
  return 0.5 * (lo + hi);
}

}  // namespace

int main(int argc, char** argv) {
  const std::string outputPath = argc > 1 ? argv[1] : "voxel_demo.ppm";
  ImageSize outputSize {1920, 1080};
  try {
    outputSize = argc > 2 ? parseImageSizeArg(argv[2]) : ImageSize {1920, 1080};
  } catch (const std::exception& ex) {
    std::cerr << ex.what() << "\n";
    std::cerr << "usage: voxel_demo [output.ppm] [size|WIDTHxHEIGHT]\n";
    return 1;
  }

  hyd_array_t* voxels = hyd_export_voxel_scene();
  if (voxels == nullptr) {
    std::cerr << "hyd_export_voxel_scene returned null\n";
    return 1;
  }

  if (hyd_array_ndims(voxels) != 3) {
    std::cerr << "expected a 3D voxel grid, got " << hyd_array_ndims(voxels) << " dimensions\n";
    hyd_array_free(voxels);
    return 1;
  }

  const int nz = static_cast<int>(hyd_array_dim(voxels, 0));
  const int ny = static_cast<int>(hyd_array_dim(voxels, 1));
  const int nx = static_cast<int>(hyd_array_dim(voxels, 2));
  const double* data = hyd_array_data_f64(voxels);

  const int outWidth = outputSize.width > 0 ? outputSize.width : nx;
  const int outHeight = outputSize.height > 0 ? outputSize.height : ny;
  std::vector<RGB> pixels(static_cast<std::size_t>(outWidth * outHeight));
  std::vector<double> luminance(static_cast<std::size_t>(outWidth * outHeight), 0.0);
  double maxLuminance = 0.0;
  const Vec3 lightDir = normalize(Vec3 {0.45, -0.35, 0.82});
  const double maxDim = static_cast<double>(std::max({nx, ny, nz}));
  const double halfX = 0.5 * static_cast<double>(nx);
  const double halfY = 0.5 * static_cast<double>(ny);
  const double halfZ = 0.5 * static_cast<double>(nz);
  const Vec3 boxMin {0.0, 0.0, 0.0};
  const Vec3 boxMax {static_cast<double>(nx - 1), static_cast<double>(ny - 1), static_cast<double>(nz - 1)};
  const Vec3 target {halfX + 4.0, halfY - 9.0, halfZ + 2.0};
  const Vec3 cameraPos {halfX - 0.94 * nx, halfY - 0.16 * ny, halfZ - 1.08 * nz};
  const Vec3 forward = normalize(target - cameraPos);
  const Vec3 right = normalize(cross(forward, Vec3 {0.0, 1.0, 0.0}));
  const Vec3 up = normalize(cross(right, forward));
  const double coarseStep = 0.55;
  const double mediumStep = 0.28;
  const double fineStep = 0.12;
  const double focalScale = 0.63;
  const double surfaceThreshold = 0.42;
  const double aspect = static_cast<double>(outWidth) / static_cast<double>(outHeight);

  for (int y = 0; y < outHeight; ++y) {
    for (int x = 0; x < outWidth; ++x) {
      const double sx = ((static_cast<double>(x) + 0.5) / outWidth - 0.5) * aspect * 1.05;
      const double sy = ((static_cast<double>(y) + 0.5) / outHeight - 0.5) * 1.05;
      const Vec3 rayDir = normalize(forward + focalScale * sx * right - focalScale * sy * up);
      double tEnter = 0.0;
      double tExit = 0.0;
      if (!intersectBox(cameraPos, rayDir, boxMin, boxMax, tEnter, tExit)) {
        continue;
      }
      const double jitter = pixelJitter(x, y);
      double accumColor = 0.0;
      double maxDensity = 0.0;
      double prevT = 0.0;
      double prevDensity = 0.0;
      bool havePrev = false;
      bool hitSurface = false;
      double t = std::max(0.0, tEnter) + jitter * coarseStep;
      while (t <= tExit) {
        const Vec3 samplePos = cameraPos + t * rayDir;
        const double density = sampleTrilinear(data, nx, ny, nz, samplePos.x, samplePos.y, samplePos.z);
        maxDensity = std::max(maxDensity, density);
        if (havePrev && prevDensity < surfaceThreshold && density >= surfaceThreshold) {
          const double hitT = refineSurfaceHit(data, nx, ny, nz, cameraPos, rayDir, prevT, t, surfaceThreshold);
          const Vec3 hitPos = cameraPos + hitT * rayDir;
          const int gx = std::clamp(static_cast<int>(std::round(hitPos.x)), 0, nx - 1);
          const int gy = std::clamp(static_cast<int>(std::round(hitPos.y)), 0, ny - 1);
          const int gz = std::clamp(static_cast<int>(std::round(hitPos.z)), 0, nz - 1);
          const Vec3 grad = sampleGradient(data, nx, ny, nz, gx, gy, gz);
          const Vec3 normal = normalize(grad);
          const double diffuse = std::max(0.0, dot(normal, lightDir));
          const double shadow = sampleShadow(data, nx, ny, nz, hitPos, lightDir);
          const double rim = std::pow(std::max(0.0, 1.0 - dot(normal, -1.0 * rayDir)), 2.0);
          const Vec3 halfVec = normalize(lightDir - rayDir);
          const double specular = std::pow(std::max(0.0, dot(normal, halfVec)), 22.0);
          const double depthFade = std::clamp(1.0 - hitT / (tExit + 1.0e-6), 0.35, 1.0);
          const double shade =
            depthFade * (0.14 + shadow * (0.18 + 0.82 * diffuse) + 0.18 * rim + 0.22 * specular);
          const double edge = std::clamp((density - surfaceThreshold) / 0.55, 0.0, 1.0);
          accumColor = shade * (0.62 + 0.38 * edge);
          hitSurface = true;
          break;
        }
        const double proximity = std::max(prevDensity, density);
        prevT = t;
        prevDensity = density;
        havePrev = true;
        const double stepSize =
          proximity >= surfaceThreshold * 0.75 ? fineStep :
            (proximity >= surfaceThreshold * 0.25 ? mediumStep : coarseStep);
        t += stepSize;
      }
      if (!hitSurface) {
        const double glow = std::clamp(maxDensity / surfaceThreshold, 0.0, 1.0);
        accumColor = 0.12 * glow * glow;
      }
      const std::size_t dstIdx = static_cast<std::size_t>(y * outWidth + x);
      luminance[dstIdx] = accumColor;
      maxLuminance = std::max(maxLuminance, accumColor);
    }
  }

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
    hyd_array_free(voxels);
    return 1;
  }

  hyd_array_free(voxels);
  std::cout << "Wrote " << outputPath << " (" << outWidth << "x" << outHeight << ")\n";
  return 0;
}
