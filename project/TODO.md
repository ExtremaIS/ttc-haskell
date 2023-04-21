# `ttc-haskell` TODO

## Functionality

* [linear-builder][] support requires GHC 9.2 or later

[linear-builder]: <https://github.com/Bodigrim/linear-builder>

## Documentation

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
