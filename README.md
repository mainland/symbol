# The `symbol` Package  [![Hackage](https://img.shields.io/hackage/v/symbol.svg)](https://hackage.haskell.org/package/symbol) [![Actions Status: haskell-ci](https://github.com/mainland/symbol/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/symbol/actions?query=workflow%3Ahaskell-ci)

Provides a `Symbol` data type allowing fast symbol comparisons and functions for interning symbols and recovering their `String` representation.

See the [Data.Symbol API documentation](https://hackage.haskell.org/package/symbol/docs/Data-Symbol.html)
for examples, ordering semantics, and memory lifetime.

To build and run the test suite:

```sh
cabal build all
cabal test all --test-show-details=direct
```

To build the API documentation locally:

```sh
cabal haddock
```

The package supports GHC 8.0 and later. CI tests the versions listed in
`symbol.cabal`. Regenerate the workflow with `haskell-ci regenerate` after
changing that list or the package components.

Formatting uses `.stylish-haskell.yaml`. With the VS Code Haskell extension and
a working Haskell Language Server, the workspace settings enable formatting on
save. To format from the command line:

```sh
stylish-haskell -i Setup.hs src/Data/Symbol.hs src/Data/Symbol/Unsafe.hs tests/Main.hs
```

## AI assistance

All contributions included in releases through version `0.2.4.1` were made
without AI assistance. AI tools were used in development, review, and release
preparation after that version, through version `1.0.0`. The maintainer remains
responsible for this release.
