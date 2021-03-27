# `ttc-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.0.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## Unreleased

### Breaking

* Add support for GHC 9
* Add `renderTLB`, `renderBSB`, and `renderSBS` functions
* Use `Textual` error messages for `parseEnum'`

### Non-Breaking

* Add `@since` annotations
* Rename Git default branch to `main`
* Use GitHub Actions instead of Travis CI
* Add Cabal tests to GitHub Actions
* Add [stan](https://hackage.haskell.org/package/stan) static analysis

## 0.3.0.0 (2020-11-03)

### Breaking

* Use `Textual` error messages
* Add `maybeParseWithRead` function

## 0.2.3.0 (2020-09-25)

### Non-Breaking

* Bump `bytestring` dependency version upper bound

## 0.2.2.0 (2020-05-17)

### Non-Breaking

* Bump `template-haskell` dependency version upper bound
    * Update `lift` example for compatibility with `template-haskell 2.16.0.0`

## 0.2.1.0 (2020-05-11)

### Non-Breaking

* Update examples to support older libraries
* Refactor `Makefile`, add `STACK_NIX_PATH` support
* Add `test-all` command to `Makefile`
* Bump `tasty` dependency version upper bound

## 0.2.0.0 (2019-12-15)

### Non-Breaking

* Add untyped validation functions
* Move examples to a separate package
* Refactor examples and add more

## 0.1.0.1 (2019-12-02)

### Non-Breaking

* Bump `time` dependency version upper bound

## 0.1.0.0 (2019-12-01)

### Non-Breaking

* Update Cabal file in preparation for release to Hackage

## 0.0.0.4 (2019-11-30)

### Non-Breaking

* Update Cabal file in preparation for release to Hackage
* Update documentation
* Add examples

## 0.0.0.3 (2019-11-28)

### Non-Breaking

* Add continuous integration support

## 0.0.0.2 (2019-11-28)

### Non-Breaking

* Update Cabal metadata
* Update README

## 0.0.0.1 (2019-11-23)

### Breaking

* Initial public release
