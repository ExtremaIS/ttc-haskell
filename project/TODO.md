# `ttc-haskell` TODO

## Functionality

## Documentation

* Add simple examples to the README.
* Add simple examples to the API documentation.

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

* Setup Travis CI.
* Add to Hackage.
* Add to Stackage.
