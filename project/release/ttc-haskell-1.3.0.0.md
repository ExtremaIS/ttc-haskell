# `ttc-haskell` `1.3.0.0` Release Notes

Date
: 2023-09-17

## Overview

TTC, an initialism of _Textual Type Classes_, is a library that provides the
following functionality:

* The `Textual` type class is used to convert between common textual data
  types.  It can be used to write functions that accept or return values of
  any of these textual data types.
* The `Render` type class is used to render a value as text.  Avoid bugs by
  only using `Show` for debugging/development purposes.
* The `Parse` type class is used to parse a value from text.  Unlike `Read`,
  it has support for error messages.
* Validate constants at compile-time using `Parse` instances.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/ttc-haskell#readme>

## This Release

This release adds support for validating constants at compile-time without
having to type `valid` and adds a new set of `Parse` functions.

### Major Changes

#### Minimal Compile-Time Validation Syntax

The `valid` function uses a `Parse` instance to validate a constant at
compile-time, requiring that the parsed type has a `Lift` instance.  This
release adds an `IsString` instance for typed Template Haskell expressions so
that string syntax automatically calls `valid`.  See the orphan instances
section of the documentation for details and usage examples.

#### New `Parse` Functions

The `parseOrFail` set of functions fail using `MonadFail` on error.

### Compatibility

TTC is currently tested with [GHC 8.2.2][] through [GHC 9.6.2][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - ttc-1.3.0.0
```

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[GHC 9.6.2]: <https://www.haskell.org/ghc/download_ghc_9_6_2.html>

### Issues

There are no known issues at this time.
