# `ttc-haskell` TODO

## Functionality

* [Orphan instances for core types](https://github.com/ExtremaIS/ttc-haskell/issues/1)
* [Text Builder, ByteString Builder, ShortByteString support in Textual](https://github.com/ExtremaIS/ttc-haskell/issues/2)

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
