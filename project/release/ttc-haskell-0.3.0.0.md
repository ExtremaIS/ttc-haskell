# `ttc-haskell` `0.3.0.0` Release Notes

Date
: 2020-11-03

## Overview

This is a major release of TTC.  The `Parse` type class now uses `Textual`
error messages, and the following function is added:

* `maybeParseWithRead`

To migrate existing code, `Parse` instances must be changed to return
`Textual` errors instead of `String` errors.  Calls to `parse` functions may
be refactored to make use of the `Textual` error messages.  If a call to a
`parse` function breaks with an arbitrary type error, it may be an indicator
that the error message is not used; consider using a `parseMaybe` function
instead.

This release should contain all of the API changes for the upcoming `1.0.0.0`
release.
