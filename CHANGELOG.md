# Changelog

## Unreleased

- Add GHC 9.12 and 9.14 to CI and allow `containers` 0.8.
- Fix generic `Data` operations to reconstruct symbols through `intern`,
  preserving the association between identifiers and strings. **Compatibility
  change:** the generic representation now has one `String` field instead of
  `Int` and `String` fields. Consumers of the old representation must adapt.
- Require `base >= 4.9`, matching the GHC 8.0+ CI matrix, and remove obsolete
  compiler compatibility code.
- Add baseline and regression tests, with test execution in CI.
- Move library sources into `src/`, enable library warnings, and configure
  Stylish Haskell and VS Code formatting.
- Document ordering, interned-string lifetime, and the generic representation.
