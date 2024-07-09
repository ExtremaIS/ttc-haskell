# TTC: Textual Type Classes

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/ttc-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/ttc-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/ttc.svg)](https://hackage.haskell.org/package/ttc)
[![Stackage LTS](https://stackage.org/package/ttc/badge/lts)](https://stackage.org/package/ttc)
[![Stackage Nightly](https://stackage.org/package/ttc/badge/nightly)](https://stackage.org/nightly/package/ttc)

## Overview

TTC, an initialism of _Textual Type Classes_, is a library that provides
`Render` and `Parse` type classes for conversion between data types and
textual data types (strings).  Use the `Show` and `Read` type classes for
debugging/development, and use the `Render` and `Parse` type classes for your
own purposes.  The library also provides various ways to validate constants at
compile-time.

This package (`ttc`) uses a `Textual` type class for conversion between
textual data types.  This package uses simple types and compiles quickly, but
the supported textual data types are fixed.  It is not possible for users to
add support for additional textual data types.

Since a type may have at most one instance of a given type class, special care
must be taken when defining type class instances in a shared library.  In
particular, orphan instances should generally *not* be used in shared
libraries since they prevent users of the libraries from writing their own
instances.

`Render` and `Parse` are best used with types that have canonical textual
representations, such as textual identifiers.  When there is more than one way
to create a textual representation, such as configurable formatting, using a
normal function is probably more appropriate.  Such a function can make use of
the `Textual` type class to support multiple textual data types.

This overview includes a brief introduction of the library.  The following
resources are also available:

* [API documentation][] is viewable on Hackage.
* A [series of articles][] gives a guided tour of the library.
    1. [Textual Type Class][]
    2. [Render and Parse][]
    3. [Validated Constants][]
    4. [Best Practices][]
* The [`ttc-examples`][] directory in the repository contains usage examples.

[API documentation]: <https://hackage.haskell.org/package/ttc#modules>
[series of articles]: <https://www.extrema.is/articles/ttc-textual-type-classes>
[Textual Type Class]: <https://www.extrema.is/articles/ttc-textual-type-classes/textual-type-class>
[Render and Parse]: <https://www.extrema.is/articles/ttc-textual-type-classes/render-and-parse>
[Validated Constants]: <https://www.extrema.is/articles/ttc-textual-type-classes/validated-constants>
[Best Practices]: <https://www.extrema.is/articles/ttc-textual-type-classes/best-practices>
[`ttc-examples`]: <https://github.com/ExtremaIS/ttc-haskell/tree/main/ttc-examples>

### `Textual`

The `Textual` type class is used to convert between the following textual data
types:

* `String`
* Strict `Text`
* Lazy `Text`
* `Text` `Builder`
* `ShortText`
* Strict `ByteString`
* Lazy `ByteString`
* `ByteString` `Builder` (and `Data.Binary.Builder`)
* `ShortByteString`

This type class has two key features:

* Type conversion is *not* done through a fixed type (such as `String` or
  `Text`).
* It has a single type variable, making it easy to write functions that
  accept arguments and/or return values that may be any of the supported
  textual data types.

For more details, see the [Textual Type Class][] article.

### `Render`

The `Render` type class renders a data type as a `Textual` data type:

```haskell
class Render a where
  render :: Textual t => a -> t
```

It is analogous to the `Show` type class, which can be reserved for
debugging/development.

The `render` function returns any of the supported textual data types.  Use
the textual data type that is most natural in the implementation of `render`
instances, and return values are converted to other textual data types when
necessary.  The `Show` and `IsString` type classes are not used, so use of the
`String` type is not required.

As a simple example, consider a `Username` type that is implemented as a
`newtype` over `Text`:

```haskell
module Username (Username) where

import Control.Monad (unless, when)
import Data.Char (isAsciiLower)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.TTC as TTC

newtype Username = Username Text
  deriving (Eq, Ord, Show)

instance TTC.Render Username where
  render (Username t) = TTC.convert t
```

If a username needs to be included in a `String` error message, conversion is
automatic:

```haskell
putStrLn $ "user not found: " ++ TTC.render uname
```

For more details, see the [Render and Parse][] article.

### `Parse`

The `Parse` type class parses a data type from a `Textual` data type:

```haskell
class Parse a where
  parse :: (Textual t, Textual e) => t -> Either e a
```

It is analogous to the `Read` type class, which can be reserved for
debugging/development.

The `parse` function takes any of the supported textual data types as an
argument.  Use the textual data type that is most natural in the
implementation of `parse` instances, and arguments are converted from other
textual data types when necessary.  The `IsString` type class is not used, so
use of the `String` type is not required.

Here is an example instance for `Username`, implementing some restrictions:

```haskell
instance TTC.Parse Username where
  parse = TTC.asT $ \t -> TTC.prefixErrorS "invalid username: " $ do
    unless (T.all isAsciiLower t) $ Left "not only lowercase ASCII letters"
    let len = T.length t
    when (len < 3) $ Left "fewer than 3 characters"
    when (len > 12) $ Left "more than 12 characters"
    pure $ Username t
```

If a username needs to be parsed from a `String`, conversion is automatic:

```haskell
case TTC.parse s :: Either String Username of
  Right uname -> "valid username: " ++ TTC.render uname
  Left err    -> err
```

For more details, see the [Render and Parse][] article.

### Constant Validation

TTC provides functions to validate constants at compile-time, using Template
Haskell.  For example, a `Username` constant can be defined as follows:

```haskell
user :: Username
user = $$(TTC.valid "tcard")
```

For more details, see the [Validated Constants][] article.

## Project

See the [project README][] for general project information.

[project README]: <https://github.com/ExtremaIS/ttc-haskell#ttc-textual-type-classes>
