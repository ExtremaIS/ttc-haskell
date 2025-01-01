# TTC: Textual Type Classes

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/ttc-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/ttc-haskell/actions)

TTC, an initialism of _Textual Type Classes_, is a library that provides
`Render` and `Parse` type classes for conversion between data types and
textual data types (strings).  Use the `Show` and `Read` type classes for
debugging/development, and use the `Render` and `Parse` type classes for your
own purposes.  The library also provides various ways to validate constants at
compile-time.

## Packages

This repository is used to maintain multiple, related Haskell packages.

* Package `ttc` uses a `Textual` type class for conversion between textual
  data types.  This package uses simple types and compiles quickly, but the
  supported textual data types are fixed.  It is not possible for users to
  add support for additional textual data types.
* Package `ttc-examples` contains `ttc` usage examples.

## Related Work

### Rendering and Parsing

The [relude][] library has polymorphic versions of `show` and `readEither` in
`Relude.String.Conversion`, as well as various type classes for converting
between string types.  This does not encourage using `Show` and `Read`
instances with syntactically valid Haskell syntax, and it encourages the using
of the `String` data type.

The [rio][] library has a `Display` type class with a similar goal as
`TTC.Render`.  Since the library encourages a uniform usage of textual data
types, `Display` only provides functions for rendering to `Text` and a builder
format.  It does not have a type class similar to `TTC.Parse`.

The [text-display][] library defines a `Display` type class intended to render
user-facing text.  It uses a `Builder` type internally and renders to a `Text`
value.

Harry Garrood has an interesting series of blog posts about type classes and
`Show`:

* [Down with Show! Part 1: Rules of thumb for when to use a type class][]
* [Down with Show! Part 2: What's wrong with the Show type class][]
* [Down with Show! Part 3: A replacement for Show][]

[relude]: <https://hackage.haskell.org/package/relude>
[rio]: <https://hackage.haskell.org/package/rio>
[text-display]: <https://hackage.haskell.org/package/text-display>
[Down with Show! Part 1: Rules of thumb for when to use a type class]: <https://harry.garrood.me/blog/down-with-show-part-1/>
[Down with Show! Part 2: What's wrong with the Show type class]: <https://harry.garrood.me/blog/down-with-show-part-2/>
[Down with Show! Part 3: A replacement for Show]: <https://harry.garrood.me/blog/down-with-show-part-3/>

### Validating Constants

The [qq-literals][] library creates a `QuasiQuoter` from a parse function of
type `String -> Either String a`.  The functionality is similar to TTC's
`mkUntypedValidQQ` function.  The `mkUntypedValidQQ` function allows the user
to choose the name of the `QuasiQuoter` because a name like `valid` is
preferred when used via a qualified import while a name like `username` may be
preferred when not using qualified imports.  Note that `mkUntypedValidQQ` also
splices in an explicit type signature.

The [validated-literals][] library has a `Validate` type class that is similar
to `TTC.Parse` but supports conversion between arbitrary types, not just from
textual data types.  Template Haskell functions are provided to perform
validation at compile-time.  Result types must either have `Lift` instances or
equivalent implementations.

Chris Done posted [a gist][] about implementing statically checked overloaded
strings.

[qq-literals]: <https://hackage.haskell.org/package/qq-literals>
[validated-literals]: <https://hackage.haskell.org/package/validated-literals>
[a gist]: <https://gist.github.com/chrisdone/809296b769ee36d352ae4f8dbe89a364>

### String Type Conversion

There are a number of libraries that simplify conversion between string types.

The following libraries provide type classes with two type variables.  The
primary benefit of this approach is that one can add support for any string
type.  The drawback of this approach is that implementations of `Render` and
`Parse` using such a type class would have to be done via a fixed type,
resulting in unnecessary conversion when using other types.

* [string-conv][]
* [string-conversions][]

The following library provide type classes with a single type variable, but
conversion is done via a fixed type.

* [hxt-regex-xmlschema][] has a `StringLike` type class and does conversion
  via the `String` type
* [ListLike][] has a `StringLike` type class and does conversion via the
  `String` type
* [monoid-subclasses][] provides a `TextualMonoid` type class that provides an
  abstract API over textual types, using `String` as the underlying type
* [stringlike][] converts via the `Text` type
* [tagsoup][] has a `StringLike` type class that provides an abstract API over
  textual types and a `castString` function that converts via the `String`
  type
* [text-conversions][] converts via the `Text` type
* [textual][] (deprecated) converts via the `String` type

[string-conv]: <https://hackage.haskell.org/package/string-conv>
[string-conversions]: <https://hackage.haskell.org/package/string-conversions>
[hxt-regex-xmlschema]: <https://hackage.haskell.org/package/hxt-regex-xmlschema>
[ListLike]: <https://hackage.haskell.org/package/ListLike>
[monoid-subclasses]: <https://hackage.haskell.org/package/monoid-subclasses>
[stringlike]: <https://hackage.haskell.org/package/stringlike>
[tagsoup]: <https://hackage.haskell.org/package/tagsoup>
[text-conversions]: <https://hackage.haskell.org/package/text-conversions>
[textual]: <https://hackage.haskell.org/package/textual>

### Arbitrary Type Conversion

There are also a number of libraries that provide type classes for conversion
between arbitrary types, including string types.

* [basement][] provides type classes for conversion that may fail as well as
  conversion that cannot fail
* [convertible][]
* [witch][] provides type classes for conversion that may fail as well as
  conversion that cannot fail

[basement]: <https://hackage.haskell.org/package/basement>
[convertible]: <https://hackage.haskell.org/package/convertible>
[witch]: <https://hackage.haskell.org/package/witch>

## Project

### Links

* GitHub: <https://github.com/ExtremaIS/ttc-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/ttc-haskell/actions>

### Branches

The `main` branch is reserved for releases.  It may be considered stable, and
`HEAD` is always the latest release.

The `develop` branch is the primary development branch.  It contains changes
that have not yet been released, and it is not necessarily stable.

[Hackage revisions][] are made for metadata changes, such as relaxation of
constraints when new versions of dependencies are released.  The `.cabal`
metadata in the `main` branch may therefore not match that of Hackage.  The
`.cabal` metadata in the `develop` branch may match, *unless* work is being
done on a new release that contains other changes.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### Tags

All releases are tagged in the `main` branch.  Release tags are signed using
the [`security@extrema.is` GPG key][].

[`security@extrema.is` GPG key]: <https://keyserver.ubuntu.com/pks/lookup?search=0x1D484E4B4705FADF&fingerprint=on&op=index>

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/ttc-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the [MIT License][] as specified in the
[`LICENSE`][] file.

[MIT License]: <https://opensource.org/licenses/MIT>
[`LICENSE`]: <LICENSE>
