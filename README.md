# The `symbol` Package  [![Hackage](https://img.shields.io/hackage/v/symbol.svg)](https://hackage.haskell.org/package/symbol) [![Actions Status: haskell-ci](https://github.com/mainland/symbol/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/symbol/actions?query=workflow%3Ahaskell-ci)

Provides a `Symbol` data type allowing fast symbol comparisons and functions for interning symbols and recovering their `String` representation.

```haskell
import Data.Symbol (intern, unintern)

intern "name" == intern "name"  -- True
unintern (intern "name")        -- "name"
```

Symbols created through `intern` compare equal exactly when their strings do.
Equality and ordering compare integer identifiers in constant time. Ordering
follows identifier allocation, rather than lexicographic string order, and can
vary with evaluation order and between runs. For lexicographic ordering, compare
the strings returned by `unintern`.

Interning uses a synchronized, process-wide table. Every distinct interned
string and its symbol remain in that table for the lifetime of the process, even
after the caller drops all references. Memory use therefore grows with the
number and total size of distinct strings interned. `intern` fully evaluates its
input string before accessing the table. Inputs must be finite and fully
defined.

The `Data` instance exposes a `Symbol` constructor with one `String` field.
Generic construction and transformations call `intern`, preserving symbol
identity. Use `Data.Symbol` for the abstract API. `Data.Symbol.Unsafe` exposes
the raw constructor, which can break the association between identifiers and
strings.

To build and run the test suite:

```sh
cabal build all
cabal test all --test-show-details=direct
```

The package supports GHC 8.0 and later. CI tests the versions listed in
`symbol.cabal`; regenerate the workflow with `haskell-ci regenerate` after
changing that list or the package components.

Formatting uses `.stylish-haskell.yaml`. With the VS Code Haskell extension and
a working Haskell Language Server, the workspace settings enable formatting on
save. To format from the command line:

```sh
stylish-haskell -i Setup.hs src/Data/Symbol.hs src/Data/Symbol/Unsafe.hs tests/Main.hs
```
