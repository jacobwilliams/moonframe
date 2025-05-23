## Description

A Fortran library to interpolate the MOON_PA reference frame without using SPICE.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![GitHub release](https://img.shields.io/github/release/jacobwilliams/moonframe.svg)](https://github.com/jacobwilliams/moonframe/releases/latest)
[![Build Status](https://github.com/jacobwilliams/moonframe/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/moonframe/actions)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/moonframe)](https://github.com/jacobwilliams/moonframe/commits/master)

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

### Documentation


The latest API documentation for the `master` branch can be found [here](https://jacobwilliams.github.io/moonframe/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).
