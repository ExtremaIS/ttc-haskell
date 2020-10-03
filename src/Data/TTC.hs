------------------------------------------------------------------------------
-- |
-- Module      : Data.TTC
-- Description : textual type classes
-- Copyright   : Copyright (c) 2019-2020 Travis Cardwell
-- License     : MIT
--
-- TTC, an initialism of /Textual Type Classes/, is a library that provides
-- type classes for conversion between data types and textual data types
-- (strings).
--
-- This library is meant to be imported qualified, as follows:
--
-- @
-- import qualified Data.TTC as TTC
-- @
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.TTC
  ( -- * Textual
    Textual
  , convert
    -- ** \"To\" Conversions
    -- $TextualTo
  , toS
  , toT
  , toTL
  , toBS
  , toBSL
    -- ** \"From\" Conversions
    -- $TextualFrom
  , fromS
  , fromT
  , fromTL
  , fromBS
  , fromBSL
    -- ** \"As\" Conversions
    -- $TextualAs
  , asS
  , asT
  , asTL
  , asBS
  , asBSL
    -- ** Other Conversions
    -- $TextualOther
  , toTLB
  , fromTLB
  , toBSB
  , fromBSB
  , toSBS
  , fromSBS
    -- * Render
  , Render(..)
    -- ** Rendering Specific Types
    -- $RenderSpecific
  , renderS
  , renderT
  , renderTL
  , renderBS
  , renderBSL
    -- ** Render Utilities
  , renderWithShow
    -- * Parse
  , Parse(..)
    -- ** Parsing From Specific Types
    -- $ParseSpecific
  , parseS
  , parseT
  , parseTL
  , parseBS
  , parseBSL
    -- ** 'Maybe' Parsing
    -- $ParseMaybe
  , parseMaybe
  , parseMaybeS
  , parseMaybeT
  , parseMaybeTL
  , parseMaybeBS
  , parseMaybeBSL
    -- ** Unsafe Parsing
    -- $ParseUnsafe
  , parseUnsafe
  , parseUnsafeS
  , parseUnsafeT
  , parseUnsafeTL
  , parseUnsafeBS
  , parseUnsafeBSL
    -- ** Parse Utilities
  , parseEnum
  , parseEnum'
  , parseWithRead
  , parseWithRead'
  , maybeParseWithRead
  , readsEnum
  , readsWithParse
    -- ** Constant Validation
  , valid
  , validOf
  , mkValid
  , untypedValidOf
  , mkUntypedValid
  , mkUntypedValidQQ
  ) where

-- https://hackage.haskell.org/package/base
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS

-- https://hackage.haskell.org/package/template-haskell
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Q
import qualified Language.Haskell.TH.Syntax as THS

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE

-- HLint does not support typed expression quotations:
--   https://github.com/ndmitchell/hlint/issues/332
--
-- The following ignore annotation is not working.  It works via the CLI:
--   hlint -i "Parse error"
{-# ANN module "HLint: ignore Parse error" #-}

------------------------------------------------------------------------------
-- $Textual

-- | The 'Textual' type class is used to convert between the following textual
-- data types:
--
-- * 'String' (@S@)
-- * Strict 'T.Text' (@T@)
-- * Lazy 'TL.Text' (@TL@)
-- * Strict 'BS.ByteString' (@BS@)
-- * Lazy 'BSL.ByteString' (@BSL@)
--
-- @ByteString@ values are assumed to be UTF-8 encoded text.  Invalid bytes
-- are replaced with the Unicode replacement character @U+FFFD@.  In cases
-- where different behavior is required, process @ByteString@ values /before/
-- using this class.
--
-- The key feature of this type class is that it has a single type variable,
-- making it easy to write functions that accepts arguments and/or returns
-- values that may be any of the supported textual data types.
--
-- Note that support for additional data types cannot be implemented by
-- writing instances.  Adding support for additional data types would require
-- changing the class definition itself.  This is the price paid for having
-- only one type variable instead of two.
class Textual t where
  -- | Convert to a 'String'
  toS :: t -> String

  -- | Convert to strict 'T.Text'
  toT :: t -> T.Text

  -- | Convert to lazy 'TL.Text'
  toTL :: t -> TL.Text

  -- | Convert to a strict 'BS.ByteString'
  toBS :: t -> BS.ByteString

  -- | Convert to a lazy 'BS.ByteString'
  toBSL :: t -> BSL.ByteString

  -- | Convert between any supported textual data types
  convert :: Textual t' => t' -> t

instance Textual String where
  toS = id
  toT = T.pack
  toTL = TL.pack
  toBS = TE.encodeUtf8 . T.pack
  toBSL = TLE.encodeUtf8 . TL.pack
  convert = toS
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE convert #-}

instance Textual T.Text where
  toS = T.unpack
  toT = id
  toTL = TL.fromStrict
  toBS = TE.encodeUtf8
  toBSL = TLE.encodeUtf8 . TL.fromStrict
  convert = toT
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE convert #-}

instance Textual TL.Text where
  toS = TL.unpack
  toT = TL.toStrict
  toTL = id
  toBS = BSL.toStrict . TLE.encodeUtf8
  toBSL = TLE.encodeUtf8
  convert = toTL
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE convert #-}

instance Textual BS.ByteString where
  toS = T.unpack . TE.decodeUtf8With TEE.lenientDecode
  toT = TE.decodeUtf8With TEE.lenientDecode
  toTL = TLE.decodeUtf8With TEE.lenientDecode . BSL.fromStrict
  toBS = id
  toBSL = BSL.fromStrict
  convert = toBS
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE convert #-}

instance Textual BSL.ByteString where
  toS = TL.unpack . TLE.decodeUtf8With TEE.lenientDecode
  toT = TL.toStrict . TLE.decodeUtf8With TEE.lenientDecode
  toTL = TLE.decodeUtf8With TEE.lenientDecode
  toBS = BSL.toStrict
  toBSL = id
  convert = toBSL
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE convert #-}

-- $TextualTo
--
-- These functions are equivalent to 'convert', but they specify the type
-- being converted to.  Use them to avoid having to write type annotations in
-- cases where the type is ambiguous.

-- $TextualFrom
--
-- These functions are equivalent to 'convert', but they specify the type
-- being converted from.  Use them to avoid having to write type annotations
-- in cases where the type is ambiguous.

-- | Convert from a 'String'
fromS :: Textual t => String -> t
fromS = convert
{-# INLINE fromS #-}

-- | Convert from strict 'T.Text'
fromT :: Textual t => T.Text -> t
fromT = convert
{-# INLINE fromT #-}

-- | Convert from lazy 'TL.Text'
fromTL :: Textual t => TL.Text -> t
fromTL = convert
{-# INLINE fromTL #-}

-- | Convert from a strict 'BS.ByteString'
fromBS :: Textual t => BS.ByteString -> t
fromBS = convert
{-# INLINE fromBS #-}

-- | Convert from a lazy 'BSL.ByteString'
fromBSL :: Textual t => BSL.ByteString -> t
fromBSL = convert
{-# INLINE fromBSL #-}

-- $TextualAs
--
-- These functions are used to convert a 'Textual' argument of a function to a
-- specific type.  Use them to reduce boilerplate in small function
-- definitions.

-- | Convert an argument to a 'String'
asS :: Textual t => (String -> a) -> t -> a
asS f = f . convert
{-# INLINE asS #-}

-- | Convert an argument to strict 'T.Text'
asT :: Textual t => (T.Text -> a) -> t -> a
asT f = f . convert
{-# INLINE asT #-}

-- | Convert an argument to lazy 'TL.Text'
asTL :: Textual t => (TL.Text -> a) -> t -> a
asTL f = f . convert
{-# INLINE asTL #-}

-- | Convert an argument to a strict 'BS.ByteString'
asBS :: Textual t => (BS.ByteString -> a) -> t -> a
asBS f = f . convert
{-# INLINE asBS #-}

-- | Convert an argument to a lazy 'BSL.ByteString'
asBSL :: Textual t => (BSL.ByteString -> a) -> t -> a
asBSL f = f . convert
{-# INLINE asBSL #-}

-- $TextualOther
--
-- These functions are used to convert to/from the following other textual
-- data types:
--
-- * @Text@ 'TLB.Builder' (@TLB@)
-- * @ByteString@ 'BSB.Builder' (@BSB@)
-- * 'SBS.ShortByteString' (@SBS@)

-- | Convert to a @Text@ 'TLB.Builder'
toTLB :: Textual t => t -> TLB.Builder
toTLB = TLB.fromLazyText . convert

-- | Convert from a @Text@ 'TLB.Builder'
fromTLB :: Textual t => TLB.Builder -> t
fromTLB = convert . TLB.toLazyText

-- | Convert to a @ByteString@ 'BSB.Builder'
toBSB :: Textual t => t -> BSB.Builder
toBSB = BSB.lazyByteString . convert

-- | Convert from a @ByteString@ 'BSB.Builder'
fromBSB :: Textual t => BSB.Builder -> t
fromBSB = convert . BSB.toLazyByteString

-- | Convert to a 'SBS.ShortByteString'
toSBS :: Textual t => t -> SBS.ShortByteString
toSBS = SBS.toShort . convert

-- | Convert from a 'SBS.ShortByteString'
fromSBS :: Textual t => SBS.ShortByteString -> t
fromSBS = convert . SBS.fromShort

------------------------------------------------------------------------------
-- $Render

-- | The 'Render' type class renders a data type as a textual data type.
--
-- There are no default instances for the 'Render' type class, so that all
-- instances can be customized per project when desired.  Instances for some
-- basic data types are available in "Data.TTC.Instances".
--
-- See the @uname@ and @prompt@ example programs in the @examples@ directory.
class Render a where
  render :: Textual t => a -> t

-- $RenderSpecific
--
-- These functions are equivalent to 'render', but they specify the type being
-- rendered to.  Use them to avoid having to write type annotations in cases
-- where the type is ambiguous.

-- | Render to a 'String'
renderS :: Render a => a -> String
renderS = render
{-# INLINE renderS #-}

-- | Render to strict 'T.Text'
renderT :: Render a => a -> T.Text
renderT = render
{-# INLINE renderT #-}

-- | Render to lazy 'TL.Text'
renderTL :: Render a => a -> TL.Text
renderTL = render
{-# INLINE renderTL #-}

-- | Render to a strict 'BS.ByteString'
renderBS :: Render a => a -> BS.ByteString
renderBS = render
{-# INLINE renderBS #-}

-- | Render to a lazy 'BSL.ByteString'
renderBSL :: Render a => a -> BSL.ByteString
renderBSL = render
{-# INLINE renderBSL #-}

-- $RenderUtils

-- | Render a value to a textual data type using the 'Show' instance
renderWithShow :: (Show a, Textual t) => a -> t
renderWithShow = convert . show
{-# INLINE renderWithShow #-}

------------------------------------------------------------------------------
-- $Parse

-- | The 'Parse' type class parses a data type from a textual data type.
--
-- There are no default instances for the 'Parse' type class, so that all
-- instances can be customized per project when desired.  Instances for some
-- basic data types are available in "Data.TTC.Instances".
--
-- See the @uname@ and @prompt@ example programs in the @examples@ directory.
class Parse a where
  parse :: (Textual t, Textual e) => t -> Either e a

-- This function is equivalent to 'parse' with the error type fixed to
-- 'String', used internally when the error is ignored.
parse' :: (Parse a, Textual t) => t -> Either String a
parse' = parse
{-# INLINE parse' #-}

-- $ParseSpecific
--
-- These functions are equivalent to 'parse', but they specify the type being
-- parsed from.  Use them to avoid having to write type annotations in cases
-- where the type is ambiguous.

-- | Parse from a 'String'
parseS :: (Parse a, Textual e) => String -> Either e a
parseS = parse
{-# INLINE parseS #-}

-- | Parse from strict 'T.Text'
parseT :: (Parse a, Textual e) => T.Text -> Either e a
parseT = parse
{-# INLINE parseT #-}

-- | Parse from lazy 'TL.Text'
parseTL :: (Parse a, Textual e) => TL.Text -> Either e a
parseTL = parse
{-# INLINE parseTL #-}

-- | Parse from a strict 'BS.ByteString'
parseBS :: (Parse a, Textual e) => BS.ByteString -> Either e a
parseBS = parse
{-# INLINE parseBS #-}

-- | Parse from a lazy 'BSL.ByteString'
parseBSL :: (Parse a, Textual e) => BSL.ByteString -> Either e a
parseBSL = parse
{-# INLINE parseBSL #-}

-- $ParseMaybe
--
-- The 'parseMaybe' function parses to a 'Maybe' type instead of an 'Either'
-- type.  The rest of the functions are equivalent to 'parseMaybe', but they
-- specify the type being parsed from.  Use them to avoid having to write type
-- annotations in cases where the type is ambiguous.

-- | Parse to a 'Maybe' type
parseMaybe :: (Parse a, Textual t) => t -> Maybe a
parseMaybe = either (const Nothing) Just . parse'
{-# INLINE parseMaybe #-}

-- | Parse from a 'String' to a 'Maybe' type
parseMaybeS :: Parse a => String -> Maybe a
parseMaybeS = parseMaybe
{-# INLINE parseMaybeS #-}

-- | Parse from strict 'T.Text' to a 'Maybe' type
parseMaybeT :: Parse a => T.Text -> Maybe a
parseMaybeT = parseMaybe
{-# INLINE parseMaybeT #-}

-- | Parse from lazy 'TL.Text' to a 'Maybe' type
parseMaybeTL :: Parse a => TL.Text -> Maybe a
parseMaybeTL = parseMaybe
{-# INLINE parseMaybeTL #-}

-- | Parse from a strict 'BS.ByteString' to a 'Maybe' type
parseMaybeBS :: Parse a => BS.ByteString -> Maybe a
parseMaybeBS = parseMaybe
{-# INLINE parseMaybeBS #-}

-- | Parse from a lazy 'BSL.ByteString' to a 'Maybe' type
parseMaybeBSL :: Parse a => BSL.ByteString -> Maybe a
parseMaybeBSL = parseMaybe
{-# INLINE parseMaybeBSL #-}

-- $ParseUnsafe
--
-- The 'parseUnsafe' function raises an exception on error instead of using an
-- 'Either' type.  It should only be used when an error is not possible.  The
-- rest of the functions are equivalent to 'parseUnsafe', but they specify the
-- type being parsed from.  Use them to avoid having to write type annotations
-- in cases where the type is ambiguous.

-- | Unsafely parse
parseUnsafe :: (Parse a, Textual t) => t -> a
parseUnsafe = either (error . ("parseUnsafe: " ++)) id . parse
{-# INLINE parseUnsafe #-}

-- | Unsafely parse to a 'String'
parseUnsafeS :: Parse a => String -> a
parseUnsafeS = parseUnsafe
{-# INLINE parseUnsafeS #-}

-- | Unsafely parse to strict 'T.Text'
parseUnsafeT :: Parse a => T.Text -> a
parseUnsafeT = parseUnsafe
{-# INLINE parseUnsafeT #-}

-- | Unsafely parse to lazy 'TL.Text'
parseUnsafeTL :: Parse a => TL.Text -> a
parseUnsafeTL = parseUnsafe
{-# INLINE parseUnsafeTL #-}

-- | Unsafely parse to a strict 'BS.ByteString'
parseUnsafeBS :: Parse a => BS.ByteString -> a
parseUnsafeBS = parseUnsafe
{-# INLINE parseUnsafeBS #-}

-- | Unsafely parse to a lazy 'BSL.ByteString'
parseUnsafeBSL :: Parse a => BSL.ByteString -> a
parseUnsafeBSL = parseUnsafe
{-# INLINE parseUnsafeBSL #-}

-- $ParseUtils

-- | Parse a value in an enumeration
--
-- See the @enum@ example program in the @examples@ directory.
parseEnum
  :: (Bounded a, Enum a, Render a, Textual t)
  => Bool        -- ^ case-insensitive when 'True'
  -> Bool        -- ^ accept unique prefixes when 'True'
  -> e           -- ^ invalid input error
  -> e           -- ^ ambiguous input error
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseEnum allowCI allowPrefix invalidError ambiguousError t =
    let t' = norm $ toT t
    in  case [v | v <- [minBound ..], t' `match` norm (render v)] of
          [v] -> Right v
          []  -> Left invalidError
          _   -> Left ambiguousError
  where
    norm :: T.Text -> T.Text
    norm = if allowCI then T.toLower else id

    match :: T.Text -> T.Text -> Bool
    match = if allowPrefix then T.isPrefixOf else (==)

-- | Parse a value in an enumeration, with 'String' error messages
--
-- The following English error messages are returned:
--
-- * \"invalid {name}\" when there are no matches
-- * \"ambiguous {name}\" when there is more than one match
parseEnum'
  :: (Bounded a, Enum a, Render a, Textual t)
  => String           -- ^ name to include in error messages
  -> Bool             -- ^ case-insensitive when 'True'
  -> Bool             -- ^ accept unique prefixes when 'True'
  -> t                -- ^ textual input to parse
  -> Either String a  -- ^ error or parsed value
parseEnum' name allowCI allowPrefix =
    parseEnum allowCI allowPrefix ("invalid " ++ name) ("ambiguous " ++ name)
{-# INLINEABLE parseEnum' #-}

-- | Parse a value using the 'Read' instance
parseWithRead
  :: (Read a, Textual t)
  => e           -- ^ invalid input error
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseWithRead invalidError = maybe (Left invalidError) Right . readMaybe . toS
{-# INLINEABLE parseWithRead #-}

-- | Parse a value using the 'Read' instance, with 'String' error messages
--
-- The following English error message is returned:
--
-- * \"invalid {name}\" when the parse fails
parseWithRead'
  :: (Read a, Textual t, Textual e)
  => String      -- ^ name to include in error messages
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseWithRead' name = parseWithRead (fromS $ "invalid " ++ name)
{-# INLINEABLE parseWithRead' #-}

-- | Parse a value to a 'Maybe' type using the 'Read' instance
maybeParseWithRead
  :: (Read a, Textual t)
  => t           -- ^ textual input to parse
  -> Maybe a  -- ^ error or parsed value
maybeParseWithRead = readMaybe . toS

-- | Implement 'ReadS' using 'parseEnum'
--
-- This implementation expects all of the input to be consumed.
readsEnum
  :: (Bounded a, Enum a, Render a)
  => Bool  -- ^ case-insensitive when 'True'
  -> Bool  -- ^ accept unique prefixes when 'True'
  -> ReadS a
readsEnum allowCI allowPrefix s =
    case parseEnum allowCI allowPrefix () () s of
      Right v -> [(v, "")]
      Left{}  -> []
{-# INLINEABLE readsEnum #-}

-- | Implement 'ReadS' using a 'Parse' instance
--
-- This implementation expects all of the input to be consumed.
readsWithParse
  :: Parse a
  => ReadS a
readsWithParse s = case parseMaybe s of
    Just v  -> [(v, "")]
    Nothing -> []
{-# INLINEABLE readsWithParse #-}

-- $ParseValid

-- | Validate a constant at compile-time using a 'Parse' instance
--
-- This function parses the 'String' at compile-time and fails compilation on
-- error.  When valid, the result is compiled in, so the result type must have
-- a 'THS.Lift' instance.  When this is inconvenient, use one of the
-- alternative functions in this library.
--
-- This function uses a typed expression.  Typed expressions were not
-- supported in @haskell-src-exts <1.22.0@, which caused problems with
-- @hlint@.  If the issue effects you, use @hlint -i "Parse error"@ to ignore
-- parse errors or use one of the alternative functions in this library.
--
-- See the @valid@, @invalid@, and @lift@ example programs in the @examples@
-- directory.
valid
  :: (Parse a, THS.Lift a)
  => String
  -> TH.Q (TH.TExp a)
valid s = case parse s of
    Right x -> [|| x ||]
    Left err -> fail $ "Invalid constant: " ++ err

-- | Validate a constant at compile-time using a 'Parse' instance
--
-- This function requires a 'Proxy' of the result type.  Use 'mkValid' to
-- avoid having to pass a 'Proxy' during constant definition.
--
-- This function parses the 'String' at compile-time and fails compilation on
-- error.  When valid, the 'String' is compiled in, to be parsed again at
-- run-time.  Since the result is not compiled in, no 'THS.Lift' instance is
-- required.
--
-- This function uses a typed expression.  Typed expressions were not
-- supported in @haskell-src-exts <1.22.0@, which caused problems with
-- @hlint@.  If the issue effects you, use @hlint -i "Parse error"@ to ignore
-- parse errors or use 'untypedValidOf' instead.
--
-- See the @validof@ example program in the @examples@ directory.
validOf
  :: Parse a
  => Proxy a
  -> String
  -> TH.Q (TH.TExp a)
validOf proxy s = case (`asProxyTypeOf` proxy) <$> parse s of
    Right{} -> [|| parseUnsafeS s ||]
    Left err -> fail $ "Invalid constant: " ++ err

-- | Make a @valid@ function using 'validOf' for the given type
--
-- Create a @valid@ function in the module for a type in order to avoid having
-- to write a 'Proxy' when defining constants.
--
-- This function uses a typed expression.  Typed expressions were not
-- supported in @haskell-src-exts <1.22.0@, which caused problems with
-- @hlint@.  If the issue effects you, use @hlint -i "Parse error"@ to ignore
-- parse errors or use 'mkUntypedValidOf' instead.
--
-- See the @mkvalid@ example program in the @examples@ directory.
mkValid
  :: String
  -> TH.Name
  -> TH.DecsQ
mkValid funName typeName = do
    let funName' = TH.mkName funName
        resultType = pure $ TH.ConT typeName
    funType <- [t| String -> TH.Q (TH.TExp $resultType) |]
    body <- [| validOf (Proxy :: Proxy $resultType) |]
    return
      [ TH.SigD funName' funType
      , TH.FunD funName' [TH.Clause [] (TH.NormalB body) []]
      ]

-- | Validate a constant at compile-time using a 'Parse' instance
--
-- This function requires a 'Proxy' of the result type.  Use 'mkUntypedValid'
-- to avoid having to pass a 'Proxy' during constant definition.
--
-- This function parses the 'String' at compile-time and fails compilation on
-- error.  When valid, the 'String' is compiled in, to be parsed again at
-- run-time.  Since the result is not compiled in, no 'THS.Lift' instance is
-- required.
--
-- See the @uvalidof@ example program in the @examples@ directory.
untypedValidOf
  :: Parse a
  => Proxy a
  -> String
  -> TH.ExpQ
untypedValidOf proxy s = case (`asProxyTypeOf` proxy) <$> parse s of
    Right{} -> [| parseUnsafeS s |]
    Left err -> fail $ "Invalid constant: " ++ err

-- | Make a @valid@ function using 'untypedValidOf' for the given type
--
-- Create a @valid@ function in the module for a type in order to avoid having
-- to write a 'Proxy' when defining constants.
--
-- See the @mkuvalid@ example program in the @examples@ directory.
mkUntypedValid
  :: String
  -> TH.Name
  -> TH.DecsQ
mkUntypedValid funName typeName = do
    let funName' = TH.mkName funName
        resultType = pure $ TH.ConT typeName
    funType <- [t| String -> TH.ExpQ |]
    body <- [| untypedValidOf (Proxy :: Proxy $resultType) |]
    return
      [ TH.SigD funName' funType
      , TH.FunD funName' [TH.Clause [] (TH.NormalB body) []]
      ]

-- | Make a @valid@ quasi-quoter using 'untypedValidOf' for the given type
--
-- See the @uvalidqq@ example program in the @examples@ directory.
mkUntypedValidQQ
  :: String
  -> TH.Name
  -> TH.DecsQ
mkUntypedValidQQ funName typeName = do
    let funName' = TH.mkName funName
        resultType = pure $ TH.ConT typeName
    expE <- [| untypedValidOf (Proxy :: Proxy $resultType) |]
    expP <- [| error "pattern not supported" |]
    expT <- [| error "type not supported" |]
    expD <- [| error "declaration not supported" |]
    let body = TH.NormalB $ TH.RecConE 'Q.QuasiQuoter
          [ ('Q.quoteExp, expE)
          , ('Q.quotePat, expP)
          , ('Q.quoteType, expT)
          , ('Q.quoteDec, expD)
          ]
    return
      [ TH.SigD funName' $ TH.ConT ''Q.QuasiQuoter
      , TH.FunD funName' [TH.Clause [] body []]
      ]
