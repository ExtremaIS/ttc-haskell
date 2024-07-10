# TTC Examples

## `uname`

This is a minimal example of using `Render` and `Parse` instances.  A username
is parsed, rendered, and printed to the screen.

Source:

* [Username.hs](uname/Username.hs)
* [uname.hs](uname/uname.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-uname

## `prompt`

This is another example of using `Render` and `Parse` instances.  The program
prompts for credit card details, with error handling, and then prints out the
normalized values as well as information about expiration.

Source:

* [CreditCard.hs](prompt/CreditCard.hs)
* [prompt.hs](prompt/prompt.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-prompt

## `enum`

This is an example of `parseEnum` usage.  The program prints a greeting in
the specified language, with support for case-insensitive prefixes.

Source:

* [enum.hs](prompt/enum.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-enum

## `valid`

This is a minimal example of using `valid` to validate a constant at
compile-time.  The `Username` data type wraps `String` and is able to derive a
`Lift` instance automatically using the `DeriveLift` extension.  Note that
HLint is currently unable to check source that makes use of typed expression
slices.

Source:

* [Username.hs](valid/Username.hs)
* [valid.hs](valid/valid.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-valid

## `invalid`

This is a minimal example of using `valid` to validate a constant at
compile-time.  It is the same as the `valid` example except that the username
is not valid, causing a compile-time validation error.

Source:

* [Username.hs](invalid/Username.hs)
* [invalid.hs](invalid/invalid.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-invalid

The compilation fails due to the invalid username.

## `lift`

This is another example of using `valid` to validate a constant at
compile-time, where one cannot automatically deriving a `Lift` instance.

Source:

* [Username.hs](lift/Username.hs)
* [lift.hs](lift/lift.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-lift

## `validof`

This is an example of using `validOf` to validate a constant at compile-time.
The `Username` data type does not need a `Lift` instance because the `String`
is compiled in.  The `String` is parsed twice: once at compile-time for
validation, and again at run-time.

Source:

* [Username.hs](validof/Username.hs)
* [validof.hs](validof/validof.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-validof

## `mkvalid`

This is an example of using `mkValid` to make a `valid` function to validate a
constant at compile-time.

Source:

* [Username.hs](mkvalid/Username.hs)
* [mkvalid.hs](mkvalid/mkvalid.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-mkvalid

## `uvalidof`

This is an example of using `untypedValidOf` to validate a constant at
compile-time.  The `Username` data type does not need a `Lift` instance
because the `String` is compiled in.  The `String` is parsed twice: once at
compile-time for validation, and again at run-time.  Note that HLint works
fine because typed expression slices are not used.

Source:

* [Username.hs](uvalidof/Username.hs)
* [uvalidof.hs](uvalidof/uvalidof.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-uvalidof

## `mkuvalid`

This is an example of using `mkUntypedValid` to make a `valid` function to
validate a constant at compile-time.  Note that HLint works fine because typed
expression slices are not used.

Source:

* [Username.hs](mkuvalid/Username.hs)
* [mkuvalid.hs](mkuvalid/mkuvalid.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-mkuvalid

## `mkuvalidqq`

This is an example of using `mkUntypedValidQQ` to make a `valid` quasi-quoter
to validate a constant at compile-time.  Note that HLint works fine because
typed expression slices are not used.

Source:

* [Username.hs](mkuvalidqq/Username.hs)
* [mkuvalidqq.hs](mkuvalidqq/mkuvalidqq.hs)

To run the example, run the following command from the `ttc-haskell` project
directory:

    $ make ttc-example-mkuvalidqq
