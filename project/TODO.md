# `ttc-haskell` TODO

## Functionality

## Documentation

Update README:

* Rewrite to make it easier to understand, with more examples
* Add links to TTC article series
* Note that README links work on GitHub

API documentation:

* Add more examples

Version `1.0.0.0` release notes:

* GHC compatibility, with Stack instructions

## Examples

`aeson` example:

```haskell
renderJsonString
  :: TTC.Render a
  => a
  -> A.Value
renderJsonString = A.String . TTC.render

parseJsonString
  :: TTC.Parse a
  => String
  -> A.Value
  -> AT.Parser a
parseJsonString expected = A.withText expected $ either fail pure . TTC.parse
```

## Project

* Stackage links on Hackage
    * <https://github.com/haskell/hackage-server/issues/921>
    * <https://github.com/commercialhaskell/stackage/issues/5963>
