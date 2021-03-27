# `ttc-haskell` `0.4.0.0` Release Notes

Date
: 2021-03-27

## Overview

This is a major release of TTC, adding support for GHC 9.

The following functions are added:

* `renderTLB` renders to a `Text` `Builder`.
* `renderBSB` renders to a `ByteString` `Builder`.
* `renderSBS` renders to a `ShortByteString`.

The following bugs are fixed:

* `parseEnum'` is changed to return `Textual` error messages.

To migrate existing code, only uses of `parseEnum'` need to be updated.
