# `ttc-haskell` `1.2.1.0` Release Notes

Date
: 2023-03-21

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

TTC `1.2.0.0` was released more than one year ago, and a number of updates to
dependency constraints have since been registered as [Hackage revisions][].
This maintenance release updates the package (tarball and `main` branch) to
the latest state.

This release also includes changes to the project management infrastructure.
One important change is that both lower and upper bounds of dependencies are
now tested in CI.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### Compatibility

Build software:

| Software          | TTC 1.2.0.0   | TTC 1.2.1.0   |
| ----------------- | ------------- | ------------- |
| [GHC][]           | 8.2.2 ~ 9.2.1 | 8.2.2 ~ 9.6.1 |
| [cabal-install][] | 1.24 ~ 3.6    | 1.24 ~ 3.10   |

Library dependencies:

| Package              | TTC 1.2.0.0         | TTC 1.2.1.0         |
| -------------------- | ------------------- | ------------------- |
| [base][]             | `>=4.7 && <5`       | `>=4.10.1 && <4.19` |
| [bytestring][]       | `>=0.10.8 && <0.12` | `>=0.10.8 && <0.12` |
| [template-haskell][] | `>=2.12 && <2.19`   | `>=2.12 && <2.21`   |
| [text][]             | `>=1.2.3 && <2.1`   | `>=1.2.2 && <2.1`   |

Test dependencies:

| Package         | TTC 1.2.0.0       | TTC 1.2.1.0      |
| --------------- | ----------------- | ---------------- |
| [tasty][]       | `>=1.0 && <1.5`   | `>=0.11 && <1.5` |
| [tasty-hunit][] | `>=0.10 && <0.11` | `>=0.8 && <0.11` |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - ttc-1.2.1.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[base]: <https://hackage.haskell.org/package/base>
[bytestring]: <https://hackage.haskell.org/package/bytestring>
[template-haskell]: <https://hackage.haskell.org/package/template-haskell>
[text]: <https://hackage.haskell.org/package/text>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>

### Issues

There are no known issues at this time.
