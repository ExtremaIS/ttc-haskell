# `ttc-haskell` `1.1.0.2` Release Notes

Date
: 2021-08-23

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

See the [README](https://github.com/ExtremaIS/ttc-haskell#readme) for details.

## This Release

This release makes changes for compatibility with GHC 9.2.1
([rc1](https://discourse.haskell.org/t/ghc-9-2-1-rc1-is-now-available/2915)).
There are no changes to the TTC API.

### Compatibility

TTC is currently tested with [GHC 8.2.2][] ([Stackage LTS 11.22][]) through
[GHC 9.0.1][].  The `.cabal` file uses Cabal version 1.24 (included with
GHC 8.2.2), so it should build fine on relatively old Haskell installations
as well as current installations.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - ttc-1.1.0.2
```

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[Stackage LTS 11.22]: <https://www.stackage.org/lts-11.22>
[GHC 9.0.1]: <https://www.haskell.org/ghc/download_ghc_9_0_1.html>

### Issues

There are no known issues at this time.
