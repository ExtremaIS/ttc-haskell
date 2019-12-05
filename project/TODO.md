# `ttc-haskell` TODO

## Functionality

## Documentation

* Add example references to the API documentation.
* Add link to `examples/README.md` to `README.md`.

## Examples

* Reorganize examples
    * Split examples into separate source directories, with no shared code
    * Minimal examples for each feature
    * Small examples
    * Guide in `examples/README.md`

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

* Add to Stackage.
