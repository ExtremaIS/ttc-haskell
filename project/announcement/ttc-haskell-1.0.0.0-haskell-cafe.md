From: Travis Cardwell <travis.cardwell@extrema.is>
To: Haskell Cafe <haskell-cafe@haskell.org>
Subject: [ANN] ttc-1.0.0.0 - Textual Type Classes

I am happy to announce the 1.0 release of TTC (Textual Type Classes).

The library provides the following functionality:

* The Textual type class is used to convert between common textual data
  types.  It can be used to write functions that accept or return values
  of any of these textual data types.

* The Render type class is used to render a value as text.  Avoid bugs
  by only using Show for debugging/development purposes.

* The Parse type class is used to parse a value from text.  Unlike Read,
  it has support for error messages.

* Validate constants at compile-time using Parse instances.

Links:

* GitHub: https://github.com/ExtremaIS/ttc-haskell
* Hackage: https://hackage.haskell.org/package/ttc
* Guided Tour: https://www.extrema.is/articles/ttc-textual-type-classes

Best regards,

Travis

----

Post: <https://mail.haskell.org/pipermail/haskell-cafe/2021-June/134046.html>
