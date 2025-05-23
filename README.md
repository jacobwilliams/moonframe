
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

The use the library, SPICELIB is not required. It can be imported like any normaal FPM library like so:

```toml
[dependencies]
moonframe = { git="https://github.com/jacobwilliams/moonframe.git" }
```
