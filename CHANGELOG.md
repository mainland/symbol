# Changelog

## 1.0.0

- Add an `NFData` instance for `Symbol` that fully evaluates the string.
- Mark the package as stable. Code written for 0.3 needs no changes unless it
  defines its own `NFData Symbol` instance.
- Expand the `Data.Symbol` documentation with examples, equality and ordering
  semantics, evaluation requirements, and the role of `Data.Symbol.Unsafe`.
  The README now points to the API documentation.
- Update the maintainer address and copyright years, and add the issue tracker
  to the package metadata.
- Raise the `containers` and `deepseq` lower bounds to 0.5.7.1 and 1.4.2.0, the
  versions that ship with GHC 8.0, and require `tasty-quickcheck >= 0.10.1` for
  the tests.
- Test with the latest GHC patch releases, keep CI on Ubuntu Jammy when the
  workflow is regenerated, and expand tabs when formatting with Stylish
  Haskell.

## 0.3.0

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
