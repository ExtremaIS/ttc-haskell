# `ttc-haskell` TODO

## Functionality

## Documentation

Add performance documentation to `parseEnum`.  A linear algorithm is used for
two reasons:

* avoid adding dependencies to the package
* most instances likely have few choices anyway

Add links to TTC article series to the README.

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
