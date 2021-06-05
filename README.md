# TTC: Textual Type Classes

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/ttc-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/ttc-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/ttc.svg)](https://hackage.haskell.org/package/ttc)
[![Stackage LTS](https://stackage.org/package/ttc/badge/lts)](https://stackage.org/package/ttc)
[![Stackage Nightly](https://stackage.org/package/ttc/badge/nightly)](https://stackage.org/nightly/package/ttc)

* [Overview](#overview)
    * [`Textual`](#textual)
    * [`Render`](#render)
    * [`Parse`](#parse)
    * [Constant Validation](#constant-validation)
* [Related Work](#related-work)
    * [Rendering and Parsing](#rendering-and-parsing)
    * [Validating Constants](#validating-constants)
    * [String Type Conversion](#string-type-conversion)
* [Project](#project)
    * [Links](#links)
    * [Dependencies](#dependencies)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

TTC, an initialism of _Textual Type Classes_, is a library that provides
`Render` and `Parse` type classes for conversion between data types and
textual data types (strings).  Use the `Show` and `Read` type classes for
debugging/development, and use the `Render` and `Parse` type classes for your
own purposes.  The library also provides a `Textual` type class for conversion
between textual data types as well as functions for validating constants at
compile-time.

This overview includes a brief introduction of the library.  The following
resources are also available:

* [API documentation][] is viewable on Hackage.
* A [series of articles][] gives a guided tour of the library.
    1. [Textual Type Class][]
    2. [Render and Parse][]
    3. [Validated Constants][]
    4. [Best Practices][]
* The [examples][] directory in the GitHub repository contains usage examples.

[API documentation]: <https://hackage.haskell.org/package/ttc#modules>
[series of articles]: <https://www.extrema.is/articles/ttc-textual-type-classes>
[Textual Type Class]: <https://www.extrema.is/articles/ttc-textual-type-classes/textual-type-class>
[Render and Parse]: <https://www.extrema.is/articles/ttc-textual-type-classes/render-and-parse>
[Validated Constants]: <https://www.extrema.is/articles/ttc-textual-type-classes/validated-constants>
[Best Practices]: <https://www.extrema.is/articles/ttc-textual-type-classes/best-practices>
[examples]: <https://github.com/ExtremaIS/ttc-haskell/tree/main/examples>

### `Textual`

The `Textual` type class is used to convert between the following textual data
types:

* `String`
* Strict `Text`
* Lazy `Text`
* Strict `ByteString`
* Lazy `ByteString`

Conversion between any of these types is direct; it is not done through a
fixed textual data type (such as `String` or `Text`).  The key feature of this
type class is that it has a single type variable, making it easy to write
functions that accept arguments and/or returns values that may be any of the
supported textual data types.

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
  parse = TTC.asT $ \t -> first TTC.fromS $ do
    unless (T.all isAsciiLower t) $ Left "username has invalid character(s)"
    let len = T.length t
    when (len < 3) $ Left "username has fewer than 3 characters"
    when (len > 12) $ Left "username has more than 12 characters"
    pure $ Username t
```

If a username needs to be parsed from a `String`, conversion is automatic:

```haskell
case TTC.parse "tcard" :: Either String Username of
  Right uname -> putStrLn $ "valid username: " ++ TTC.render uname
  Left err -> putStrLn $ "invalid username: " ++ err
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

## Related Work

### Rendering and Parsing

The [relude](https://hackage.haskell.org/package/relude) library has
polymorphic versions of `show` and `readEither` in `Relude.String.Conversion`,
as well as various type classes for converting between string types.  This
does not encourage using `Show` and `Read` instances with syntactically valid
Haskell syntax, and it encourages the using of the `String` data type.

The [rio](https://hackage.haskell.org/package/rio) library has a `Display`
type class with a similar goal as `TTC.Render`.  Since the library encourages
a uniform usage of textual data types, `Display` only provides functions for
rendering to `Text` and a builder format.  It does not have a type class
similar to `TTC.Parse`.

Harry Garrood has an interesting series of blog posts about type classes and
`Show`:

* [Down with Show! Part 1: Rules of thumb for when to use a type class](https://harry.garrood.me/blog/down-with-show-part-1/)
* [Down with Show! Part 2: What's wrong with the Show type class](https://harry.garrood.me/blog/down-with-show-part-2/)
* [Down with Show! Part 3: A replacement for Show](https://harry.garrood.me/blog/down-with-show-part-3/)

### Validating Constants

The
[validated-literals](https://hackage.haskell.org/package/validated-literals)
library has a `Validate` type class that is similar to `TTC.Parse` but
supports conversion between arbitrary types, not just from textual data types.
Template Haskell functions are provided to perform validation at compile-time.
Result types must either have `Lift` instances or equivalent implementations.

Chris Done posted
[a gist](https://gist.github.com/chrisdone/809296b769ee36d352ae4f8dbe89a364)
about implementing statically checked overloaded strings.

### String Type Conversion

There are a number of libraries that simplify conversion between string types.

The
[string-conversions](https://hackage.haskell.org/package/string-conversions)
and [string-conv](https://hackage.haskell.org/package/string-conv) libraries
have type classes with two type variables.  The primary benefit of this
approach is that one can add support for any string type.

The [text-conversions](https://hackage.haskell.org/package/text-conversions)
library converts between string types via the `Text` type, using `FromText`
and `ToText` type classes.  This works well in most cases, but it not optimal
when converting between `ByteString` types.

The [textual](https://hackage.haskell.org/package/textual) library
(deprecated) converts between string types via the `String` type, using a
`Textual` type class (which provides a `toString` function) as well as the
standard `IsString` type class (which provides the `fromString` function).

## Project

### Links

* Hackage: <https://hackage.haskell.org/package/ttc>
* Stackage: <https://stackage.org/package/ttc>
* GitHub: <https://github.com/ExtremaIS/ttc-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/ttc-haskell/actions>

### Dependencies

Dependency version bounds are strictly specified according to what versions
have been tested.  If upper bounds need to be bumped when a new package is
released or the package has been tested with earlier versions, feel free to
submit an issue.

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/ttc-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
