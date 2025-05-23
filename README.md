## Description

A Fortran library to interpolate the MOON_PA reference frame without using SPICE.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![GitHub release](https://img.shields.io/github/release/jacobwilliams/moonframe.svg)](https://github.com/jacobwilliams/moonframe/releases/latest)
[![Build Status](https://github.com/jacobwilliams/moonframe/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/moonframe/actions)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/moonframe)](https://github.com/jacobwilliams/moonframe/commits/master)

### Description

The `MOON_PA` reference frame is a high-accuracy Moon body-fixed frame. This library provides a means to use this frame without having to load SPICE kernels. This is done by splining a table of precomputed roll, pitch, and yaw values. These values are used to compute the rotating matrix from J2000 to the frame. This package also provides the code for generating these tables using SPICELIB.

### Usage

To generate the files and run the tests, this requires SPICELIB to be present and the `HAS_SPICELIB` directive to be set.

To generate the coefficient files:
```
fpm run generate --profile release --flag "-D HAS_SPICELIB" --link-flag "./toolkit/lib/spicelib.a"
```

To run the tests:
```
fpm test --profile release --flag "-D HAS_SPICELIB" --link-flag "./toolkit/lib/spicelib.a"
```

The use the library, SPICELIB is not required. It can be imported like any normal FPM library like so:

```toml
[dependencies]
moonframe = { git="https://github.com/jacobwilliams/moonframe.git" }
```

See the `interp_test.f90` for an example of how to use the library.

### Documentation


The latest API documentation for the `master` branch can be found [here](https://jacobwilliams.github.io/moonframe/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### References
 * [“High Accuracy” Orientation and Body-fixed Frames for the Moon and Earth](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/Tutorials/pdf/individual_docs/23_lunar-earth_pck-fk.pdf)