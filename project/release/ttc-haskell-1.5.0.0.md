# `ttc-haskell` `1.5.0.0` Release Notes

Date
: 2025-01-02

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

A new generation of this library is under development.  It will be released as
a separate package, and TTC will still be maintained, so TTC users will not be
forced to upgrade.  Some features are being backported to TTC, when possible.
This release contains a number of these changes, in addition to the usual
maintenance (compatibility) changes.

### Added Wrappers

Module `Data.TTC.Wrapper` defines TTC instances for `newtype` wrappers around
textual data types.  This makes it trivial to add TTC support for such data
types, using `deriving via`, reducing the temptation to just use `Show`
instances in early development of a project.

### `TypeApplications` Changes

TTC provides type-specific functions using suffixes to indicate the type, like
in `withErrorT`, so that users can avoid `TypeApplications` if desired.  This
is possible because the supported textual data types is fixed.  The new
library instead encourages use of `TypeApplications`, and the type argument
order of some functions is changes to make doing so easier.  These changes are
backported to TTC for consistency.

If you use TTC with `TypeApplications`, you may need to change the order of
type arguments in some places.

### Added Instances

Missing `RenderDefault` and `ParseDefault` instances for `TLB.Builder`,
`ST.ShortText`, `BSB.Builder`, and `SBS.ShortByteString` are added.

`RenderDefault` and `ParseDefault` instances for `Bool` are added.

### Default Instances

TTC provides default `Render` and `Parse` instances for common data types,
allowing users to optionally load them.

This release adds Template Haskell functions that can do the same thing.  One
benefit is that it is more concise when loading default instances for many
types.

### Compatibility

TTC is currently tested with [GHC 8.8.4][] through [GHC 9.12.1][].  The
`.cabal` file uses Cabal version 3.0.0.0 (included in GHC 8.8.4), so it should
build fine on relatively old Haskell installations as well as current
installations.  Note that support for software released more than five years
ago has been removed.

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - ttc-1.5.0.0
```

[GHC 8.8.4]: <https://www.haskell.org/ghc/download_ghc_8_8_4.html>
[GHC 9.12.1]: <https://www.haskell.org/ghc/download_ghc_9_12_1.html>

### Issues

There are no known issues at this time.
