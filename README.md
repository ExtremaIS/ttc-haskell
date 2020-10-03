# TTC: Textual Type Classes

[![Build Status](https://travis-ci.com/ExtremaIS/ttc-haskell.svg?branch=master)](https://travis-ci.com/ExtremaIS/ttc-haskell)
[![Hackage](https://img.shields.io/hackage/v/ttc.svg)](https://hackage.haskell.org/package/ttc)
[![Stackage LTS](https://stackage.org/package/ttc/badge/lts)](https://stackage.org/package/ttc)
[![Stackage Nightly](https://stackage.org/package/ttc/badge/nightly)](https://stackage.org/nightly/package/ttc)

* [Overview](#overview)
    * [`Render`](#render)
    * [`Parse`](#parse)
    * [`Textual`](#textual)
* [Related Work](#related-work)
    * [Rendering and Parsing](#rendering-and-parsing)
    * [Constant Validation](#constant-validation)
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
own purposes.

The following is a brief overview of the type classes provided by this
library.  See the
[API documentation on Hackage](https://hackage.haskell.org/package/ttc#modules)
for details and the [`examples` directory](examples) for usage examples.

### `Render`

The `Render` type class renders a data type as a [`Textual`](#textual) data
type:

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

### `Parse`

The `Parse` type class parses a data type from a [`Textual`](#textual) data
type:

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
  parse = TTC.asT $ \t-> first TTC.fromS $ do
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

It is common to create data types that have "smart constructors" to ensure
that all constructed values are valid.  If the `Username` constructor were
exported, it would be possible to create values with arbitrary `Text`, such
as `Username T.empty`, which is not a valid `Username`.  Smart constructors
can be inconvenient when constructing constants, however, as neither runtime
error handling nor failure are desired.  This library provides Template
Haskell functions that use `Parse` instances to validate such constants at
compile-time.

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

Functions are provided to convert to/from the following other textual data
types:

* `Text` `Builder`
* `ByteString` `Builder`
* `ShortByteString`

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

### Constant Validation

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
* Travis CI: <https://travis-ci.com/ExtremaIS/ttc-haskell>

### Dependencies

Dependency version bounds are strictly specified according to what versions
have been tested.  If upper bounds need to be bumped when a new package is
released or the package has been tested with earlier versions, feel free to
submit an issue.

### Releases

All releases are tagged in the `master` branch.  Release tags are signed using
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
