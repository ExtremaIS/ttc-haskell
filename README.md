# `ttc-haskell`

TTC, an initialism of _Textual Type Classes_, is a library that provides type
classes for conversion between data types and textual data types (strings).

* Hackage: <https://hackage.haskell.org/package/ttc> _coming soon_
* GitHub: <https://github.com/ExtremaIS/ttc-haskell>

## Overview

The following is a brief overview of the type classes provided by this
library.  See the API documentation for details and the `examples` directory
for usage examples.

### `Textual`

The `Textual` type class is used to convert between the following textual data
types:

* `String`
* Strict `Text`
* Lazy `Text`
* Strict `ByteString`
* Lazy `ByteString`

The key feature of this type class is that it has a single type variable,
making it easy to write functions that accept arguments and/or returns values
that may be any of the supported textual data types.

Functions are provided to convert to/from the following other textual data
types:

* `Text` `Builder`
* `ByteString` `Builder`
* `ShortByteString`

### `Render`

The `Render` type class renders a data type as a textual data type:

```haskell
class Render a where
  render :: Textual t => a -> t
```

Use this type class to work with strings of any format required by your
program.  It is analogous to the `Show` type class, which can be reserved
for debugging/development.

### `Parse`

The `Parse` type class parses a data type from a textual data type:

```haskell
class Parse a where
  parse :: Textual t => t -> Either String a
```

Use this type class to work with strings of any format required by your
program.  It is analogous to the `Read` type class, which can be reserved
for debugging/development.

Template Haskell functions are available to use `Parse` instances to validate
constants at compile-time.

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

Chris Done posted a gist about implementing statically checked overloaded
strings: <https://gist.github.com/chrisdone/809296b769ee36d352ae4f8dbe89a364>

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

## Contribution

Issues and feature requests are tracked on GitHub:

<https://github.com/ExtremaIS/ttc-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

## License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](/ExtremaIS/ttc-haskell/blob/master/LICENSE) file.
