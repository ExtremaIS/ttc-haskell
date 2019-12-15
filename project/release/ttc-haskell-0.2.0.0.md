# `ttc-haskell` `0.2.0.0` Release Notes

Date
: 2019-12-15

## Overview

This is a major release of TTC.  The primary change is the addition of untyped
validation functions.  There are no changes to the existing API.

The examples are now split into a separate (`ttc-examples`) package, which is
not in Hackage.  They are still in the `examples` directory, existing examples
have been refactored, and new examples have been added.  The primary reason
for using a separate package is to avoid listing example dependencies on the
Hackage page, which is misleading.  In previous versions, the `time` package
is listed as a TTC dependency, but it is only used in a (gated) example.
