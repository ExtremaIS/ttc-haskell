------------------------------------------------------------------------------
-- |
-- Module      : Data.TTC
-- Description : textual type classes
-- Copyright   : Copyright (c) 2019-2022 Travis Cardwell
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 900
{-# LANGUAGE ExplicitForAll #-}
#endif

module Data.TTC
  ( -- * Textual
    Textual
  , convert
    -- ** \"To\" Conversions
    -- $TextualTo
  , toS
  , toT
  , toTL
  , toTLB
  , toBS
  , toBSL
  , toBSB
  , toSBS
    -- ** \"From\" Conversions
    -- $TextualFrom
  , fromS
  , fromT
  , fromTL
  , fromTLB
  , fromBS
  , fromBSL
  , fromBSB
  , fromSBS
    -- ** \"As\" Conversions
    -- $TextualAs
  , asS
  , asT
  , asTL
  , asTLB
  , asBS
  , asBSL
  , asBSB
  , asSBS
    -- * Render
  , Render(..)
  , RenderDefault(..)
    -- ** Rendering Specific Types
    -- $RenderSpecific
  , renderS
  , renderT
  , renderTL
  , renderTLB
  , renderBS
  , renderBSL
  , renderBSB
  , renderSBS
    -- ** Render Utilities
  , renderWithShow
    -- * Parse
  , Parse(..)
  , ParseDefault(..)
    -- ** Parsing From Specific Types
    -- $ParseSpecific
  , parseS
  , parseT
  , parseTL
  , parseTLB
  , parseBS
  , parseBSL
  , parseBSB
  , parseSBS
    -- ** 'Maybe' Parsing
    -- $ParseMaybe
  , parseMaybe
  , parseMaybeS
  , parseMaybeT
  , parseMaybeTL
  , parseMaybeTLB
  , parseMaybeBS
  , parseMaybeBSL
  , parseMaybeBSB
  , parseMaybeSBS
    -- ** Unsafe Parsing
    -- $ParseUnsafe
  , parseUnsafe
  , parseUnsafeS
  , parseUnsafeT
  , parseUnsafeTL
  , parseUnsafeTLB
  , parseUnsafeBS
  , parseUnsafeBSL
  , parseUnsafeBSB
  , parseUnsafeSBS
    -- ** Parse With A Single Error Message
  , maybe
  , maybeS
  , maybeT
  , maybeTL
  , maybeTLB
  , maybeBS
  , maybeBSL
  , maybeBSB
  , maybeSBS
    -- ** Parse Enums
  , parseEnum
  , parseEnum'
    -- ** Read Instances
  , parseWithRead
  , parseWithRead'
  , maybeParseWithRead
  , readsEnum
  , readsWithParse
    -- ** Constant Validation
    -- $ParseValid
  , valid
  , validOf
  , mkValid
  , untypedValidOf
  , mkUntypedValid
  , mkUntypedValidQQ
  ) where

-- https://hackage.haskell.org/package/base
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import qualified Prelude
import Prelude hiding (maybe)
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

------------------------------------------------------------------------------
-- $Textual

-- | The 'Textual' type class is used to convert between the following textual
-- data types:
--
-- * 'String' (@S@)
-- * Strict 'T.Text' (@T@)
-- * Lazy 'TL.Text' (@TL@)
-- * @Text@ 'TLB.Builder' (@TLB@)
-- * Strict 'BS.ByteString' (@BS@)
-- * Lazy 'BSL.ByteString' (@BSL@)
-- * @ByteString@ 'BSB.Builder' (@BSB@) (Note: @Data.Binary.Builder@
--   re-exports this type, so TTC can be used with @binary@ as well.)
-- * 'SBS.ShortByteString' (@SBS@)
--
-- @ByteString@ values are assumed to be UTF-8 encoded text.  Invalid bytes
-- are replaced with the Unicode replacement character @U+FFFD@.  In cases
-- where different behavior is required, process @ByteString@ values /before/
-- using this class.
--
-- This type class has two key features:
--
-- * Type conversion is /not/ done through a fixed type (such as 'String' or
--   'T.Text').
-- * It has a single type variable, making it easy to write functions that
--   accept arguments and/or return values that may be any of the supported
--   textual data types.
--
-- Note that support for additional data types cannot be implemented by
-- writing instances.  Adding support for additional data types would require
-- changing the class definition itself.
--
-- For more details, see the following article:
-- <https://www.extrema.is/articles/ttc-textual-type-classes/textual-type-class>
--
-- @since 0.1.0.0
class Textual t where
  -- | Convert to a 'String'
  --
  -- @since 0.1.0.0
  toS :: t -> String

  -- | Convert to strict 'T.Text'
  --
  -- @since 0.1.0.0
  toT :: t -> T.Text

  -- | Convert to lazy 'TL.Text'
  --
  -- @since 0.1.0.0
  toTL :: t -> TL.Text

  -- | Convert to a @Text@ 'TLB.Builder'
  --
  -- @since 1.1.0.0
  toTLB :: t -> TLB.Builder

  -- | Convert to a strict 'BS.ByteString'
  --
  -- @since 0.1.0.0
  toBS :: t -> BS.ByteString

  -- | Convert to a lazy 'BS.ByteString'
  --
  -- @since 0.1.0.0
  toBSL :: t -> BSL.ByteString

  -- | Convert to a @ByteString@ 'BSB.Builder'
  --
  -- @since 1.1.0.0
  toBSB :: t -> BSB.Builder

  -- | Convert to a 'SBS.ShortByteString'
  --
  -- @since 1.1.0.0
  toSBS :: t -> SBS.ShortByteString

  -- | Convert between any supported textual data types
  --
  -- @since 0.1.0.0
  convert :: Textual t' => t' -> t

instance Textual String where
  toS = id
  toT = T.pack
  toTL = TL.pack
  toTLB = TLB.fromString
  toBS = TE.encodeUtf8 . T.pack
  toBSL = TLE.encodeUtf8 . TL.pack
  toBSB = BSB.byteString . TE.encodeUtf8 . T.pack
  toSBS = SBS.toShort . TE.encodeUtf8 . T.pack
  convert = toS
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual T.Text where
  toS = T.unpack
  toT = id
  toTL = TL.fromStrict
  toTLB = TLB.fromText
  toBS = TE.encodeUtf8
  toBSL = TLE.encodeUtf8 . TL.fromStrict
  toBSB = BSB.byteString . TE.encodeUtf8
  toSBS = SBS.toShort . TE.encodeUtf8
  convert = toT
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual TL.Text where
  toS = TL.unpack
  toT = TL.toStrict
  toTL = id
  toTLB = TLB.fromLazyText
  toBS = BSL.toStrict . TLE.encodeUtf8
  toBSL = TLE.encodeUtf8
  toBSB = BSB.lazyByteString . TLE.encodeUtf8
  toSBS = SBS.toShort . BSL.toStrict . TLE.encodeUtf8
  convert = toTL
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual TLB.Builder where
  toS = TL.unpack . TLB.toLazyText
  toT = TL.toStrict . TLB.toLazyText
  toTL = TLB.toLazyText
  toTLB = id
  toBS = BSL.toStrict . TLE.encodeUtf8 . TLB.toLazyText
  toBSL = TLE.encodeUtf8 . TLB.toLazyText
  toBSB = BSB.lazyByteString . TLE.encodeUtf8 . TLB.toLazyText
  toSBS = SBS.toShort . BSL.toStrict . TLE.encodeUtf8 . TLB.toLazyText
  convert = toTLB
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual BS.ByteString where
  toS = T.unpack . TE.decodeUtf8With TEE.lenientDecode
  toT = TE.decodeUtf8With TEE.lenientDecode
  toTL = TLE.decodeUtf8With TEE.lenientDecode . BSL.fromStrict
  toTLB = TLB.fromText . TE.decodeUtf8With TEE.lenientDecode
  toBS = id
  toBSL = BSL.fromStrict
  toBSB = BSB.byteString
  toSBS = SBS.toShort
  convert = toBS
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual BSL.ByteString where
  toS = TL.unpack . TLE.decodeUtf8With TEE.lenientDecode
  toT = TL.toStrict . TLE.decodeUtf8With TEE.lenientDecode
  toTL = TLE.decodeUtf8With TEE.lenientDecode
  toTLB = TLB.fromLazyText . TLE.decodeUtf8With TEE.lenientDecode
  toBS = BSL.toStrict
  toBSL = id
  toBSB = BSB.lazyByteString
  toSBS = SBS.toShort . BSL.toStrict
  convert = toBSL
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual BSB.Builder where
  toS =
    TL.unpack . TLE.decodeUtf8With TEE.lenientDecode . BSB.toLazyByteString
  toT =
    TL.toStrict . TLE.decodeUtf8With TEE.lenientDecode . BSB.toLazyByteString
  toTL = TLE.decodeUtf8With TEE.lenientDecode . BSB.toLazyByteString
  toTLB
    = TLB.fromLazyText
    . TLE.decodeUtf8With TEE.lenientDecode
    . BSB.toLazyByteString
  toBS = BSL.toStrict . BSB.toLazyByteString
  toBSL = BSB.toLazyByteString
  toBSB = id
  toSBS = SBS.toShort . BSL.toStrict . BSB.toLazyByteString
  convert = toBSB
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

instance Textual SBS.ShortByteString where
  toS = T.unpack . TE.decodeUtf8With TEE.lenientDecode . SBS.fromShort
  toT = TE.decodeUtf8With TEE.lenientDecode . SBS.fromShort
  toTL = TLE.decodeUtf8With TEE.lenientDecode . BSL.fromStrict . SBS.fromShort
  toTLB = TLB.fromText . TE.decodeUtf8With TEE.lenientDecode . SBS.fromShort
  toBS = SBS.fromShort
  toBSL = BSL.fromStrict . SBS.fromShort
  toBSB = BSB.byteString . SBS.fromShort
  toSBS = id
  convert = toSBS
  {-# INLINE toS #-}
  {-# INLINE toT #-}
  {-# INLINE toTL #-}
  {-# INLINE toTLB #-}
  {-# INLINE toBS #-}
  {-# INLINE toBSL #-}
  {-# INLINE toBSB #-}
  {-# INLINE toSBS #-}
  {-# INLINE convert #-}

------------------------------------------------------------------------------
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
--
-- @since 0.1.0.0
fromS :: Textual t => String -> t
fromS = convert
{-# INLINE fromS #-}

-- | Convert from strict 'T.Text'
--
-- @since 0.1.0.0
fromT :: Textual t => T.Text -> t
fromT = convert
{-# INLINE fromT #-}

-- | Convert from lazy 'TL.Text'
--
-- @since 0.1.0.0
fromTL :: Textual t => TL.Text -> t
fromTL = convert
{-# INLINE fromTL #-}

-- | Convert from a @Text@ 'TLB.Builder'
--
-- @since 1.1.0.0
fromTLB :: Textual t => TLB.Builder -> t
fromTLB = convert
{-# INLINE fromTLB #-}

-- | Convert from a strict 'BS.ByteString'
--
-- @since 0.1.0.0
fromBS :: Textual t => BS.ByteString -> t
fromBS = convert
{-# INLINE fromBS #-}

-- | Convert from a lazy 'BSL.ByteString'
--
-- @since 0.1.0.0
fromBSL :: Textual t => BSL.ByteString -> t
fromBSL = convert
{-# INLINE fromBSL #-}

-- | Convert from a @ByteString@ 'TLB.Builder'
--
-- @since 1.1.0.0
fromBSB :: Textual t => BSB.Builder -> t
fromBSB = convert
{-# INLINE fromBSB #-}

-- | Convert from a 'SBS.ShortByteString'
--
-- @since 1.1.0.0
fromSBS :: Textual t => SBS.ShortByteString -> t
fromSBS = convert
{-# INLINE fromSBS #-}

------------------------------------------------------------------------------
-- $TextualAs
--
-- These functions are used to convert a 'Textual' argument of a function to a
-- specific type.  Use them to reduce boilerplate in small function
-- definitions.

-- | Convert an argument to a 'String'
--
-- @since 0.1.0.0
asS :: Textual t => (String -> a) -> t -> a
asS f = f . convert
{-# INLINE asS #-}

-- | Convert an argument to strict 'T.Text'
--
-- @since 0.1.0.0
asT :: Textual t => (T.Text -> a) -> t -> a
asT f = f . convert
{-# INLINE asT #-}

-- | Convert an argument to lazy 'TL.Text'
--
-- @since 0.1.0.0
asTL :: Textual t => (TL.Text -> a) -> t -> a
asTL f = f . convert
{-# INLINE asTL #-}

-- | Convert an argument to a @Text@ 'TLB.Builder'
--
-- @since 1.1.0.0
asTLB :: Textual t => (TLB.Builder -> a) -> t -> a
asTLB f = f . convert
{-# INLINE asTLB #-}

-- | Convert an argument to a strict 'BS.ByteString'
--
-- @since 0.1.0.0
asBS :: Textual t => (BS.ByteString -> a) -> t -> a
asBS f = f . convert
{-# INLINE asBS #-}

-- | Convert an argument to a lazy 'BSL.ByteString'
--
-- @since 0.1.0.0
asBSL :: Textual t => (BSL.ByteString -> a) -> t -> a
asBSL f = f . convert
{-# INLINE asBSL #-}

-- | Convert an argument to a @ByteString@ 'TLB.Builder'
--
-- @since 1.1.0.0
asBSB :: Textual t => (BSB.Builder -> a ) -> t -> a
asBSB f = f . convert
{-# INLINE asBSB #-}

-- | Convert an argument to a 'SBS.ShortByteString'
--
-- @since 1.1.0.0
asSBS :: Textual t => (SBS.ShortByteString -> a) -> t -> a
asSBS f = f . convert
{-# INLINE asSBS #-}

------------------------------------------------------------------------------
-- $Render

-- | The 'Render' type class renders a data type as a textual data type.
--
-- There are no default instances for the 'Render' type class, so that all
-- instances can be customized per project when desired.  Instances for some
-- basic data types are defined for the 'RenderDefault' type class, however,
-- and you can load the 'Render' instance as follows:
--
-- @
-- instance TTC.Render Int
-- @
--
-- Since a type may have at most one instance of a given type class, special
-- care must be taken when defining type class instances in a shared library.
-- In particular, orphan instances should generally not be used in shared
-- libraries since they prevent users of the libraries from writing their own
-- instances.
--
-- See the @uname@ and @prompt@ example programs in the @examples@ directory.
--
-- For more details, see the following article:
-- <https://www.extrema.is/articles/ttc-textual-type-classes/render-and-parse>
--
-- @since 0.1.0.0
class Render a where
  render :: Textual t => a -> t

  default render :: (RenderDefault a, Textual t) => a -> t
  render = renderDefault

------------------------------------------------------------------------------

-- | The 'RenderDefault' type class provides some default 'Render' instances.
--
-- * The 'Char' instance renders a single-character string.
-- * Numeric type instances all render using the 'Show' instance.
-- * Textual type instances all convert to the target 'Textual' data type.
--
-- @since 1.1.0.0
class RenderDefault a where
  renderDefault :: Textual t => a -> t

instance RenderDefault Char where
  renderDefault c = fromS [c]

instance RenderDefault Double where
  renderDefault = renderWithShow

instance RenderDefault Float where
  renderDefault = renderWithShow

instance RenderDefault Int where
  renderDefault = renderWithShow

instance RenderDefault Int8 where
  renderDefault = renderWithShow

instance RenderDefault Int16 where
  renderDefault = renderWithShow

instance RenderDefault Int32 where
  renderDefault = renderWithShow

instance RenderDefault Int64 where
  renderDefault = renderWithShow

instance RenderDefault Integer where
  renderDefault = renderWithShow

instance RenderDefault Word where
  renderDefault = renderWithShow

instance RenderDefault Word8 where
  renderDefault = renderWithShow

instance RenderDefault Word16 where
  renderDefault = renderWithShow

instance RenderDefault Word32 where
  renderDefault = renderWithShow

instance RenderDefault Word64 where
  renderDefault = renderWithShow

instance RenderDefault String where
  renderDefault = fromS

instance RenderDefault BSL.ByteString where
  renderDefault = fromBSL

instance RenderDefault BS.ByteString where
  renderDefault = fromBS

instance RenderDefault TL.Text where
  renderDefault = fromTL

instance RenderDefault T.Text where
  renderDefault = fromT

------------------------------------------------------------------------------
-- $RenderSpecific
--
-- These functions are equivalent to 'render', but they specify the type being
-- rendered to.  Use them to avoid having to write type annotations in cases
-- where the type is ambiguous.

-- | Render to a 'String'
--
-- @since 0.1.0.0
renderS :: Render a => a -> String
renderS = render
{-# INLINE renderS #-}

-- | Render to strict 'T.Text'
--
-- @since 0.1.0.0
renderT :: Render a => a -> T.Text
renderT = render
{-# INLINE renderT #-}

-- | Render to lazy 'TL.Text'
--
-- @since 0.1.0.0
renderTL :: Render a => a -> TL.Text
renderTL = render
{-# INLINE renderTL #-}

-- | Render to a @Text@ 'TLB.Builder'
--
-- @since 0.4.0.0
renderTLB :: Render a => a -> TLB.Builder
renderTLB = render
{-# INLINE renderTLB #-}

-- | Render to a strict 'BS.ByteString'
--
-- @since 0.1.0.0
renderBS :: Render a => a -> BS.ByteString
renderBS = render
{-# INLINE renderBS #-}

-- | Render to a lazy 'BSL.ByteString'
--
-- @since 0.1.0.0
renderBSL :: Render a => a -> BSL.ByteString
renderBSL = render
{-# INLINE renderBSL #-}

-- | Render to a @ByteString@ 'BSB.Builder'
--
-- @since 0.4.0.0
renderBSB :: Render a => a -> BSB.Builder
renderBSB = render
{-# INLINE renderBSB #-}

-- | Render to a 'SBS.ShortByteString'
--
-- @since 0.4.0.0
renderSBS :: Render a => a -> SBS.ShortByteString
renderSBS = render
{-# INLINE renderSBS #-}

------------------------------------------------------------------------------
-- $RenderUtils

-- | Render a value to a textual data type using the 'Show' instance
--
-- @since 0.1.0.0
renderWithShow :: (Show a, Textual t) => a -> t
renderWithShow = convert . show
{-# INLINE renderWithShow #-}

------------------------------------------------------------------------------
-- $Parse

-- | The 'Parse' type class parses a data type from a textual data type.
--
-- There are no default instances for the 'Parse' type class, so that all
-- instances can be customized per project when desired.  Instances for some
-- basic data types are defined for the 'ParseDefault' type class, however,
-- and you can load the 'Parse' instance as follows:
--
-- @
-- instance TTC.Parse Int
-- @
--
-- Since a type may have at most one instance of a given type class, special
-- care must be taken when defining type class instances in a shared library.
-- In particular, orphan instances should generally not be used in shared
-- libraries since they prevent users of the libraries from writing their own
-- instances.
--
-- See the @uname@ and @prompt@ example programs in the @examples@ directory.
--
-- For more details, see the following article:
-- <https://www.extrema.is/articles/ttc-textual-type-classes/render-and-parse>
--
-- @since 0.3.0.0
class Parse a where
  parse :: (Textual t, Textual e) => t -> Either e a

  default parse :: (Textual t, Textual e, ParseDefault a) => t -> Either e a
  parse = parseDefault

-- This function is equivalent to 'parse' with the error type fixed to
-- 'String', used internally when the error is ignored.
--
-- @since 0.3.0.0
parse' :: (Parse a, Textual t) => t -> Either String a
parse' = parse
{-# INLINE parse' #-}

------------------------------------------------------------------------------

-- | The 'ParseDefault' type class provides some default 'Parse' instances.
--
-- * The 'Char' instance parses single-character strings.
-- * Numeric type instances all parse using the 'Read' instance.
-- * Textual type instances all convert from the source 'Textual' data type.
--
-- @since 1.1.0.0
class ParseDefault a where
  parseDefault :: (Textual t, Textual e) => t -> Either e a

instance ParseDefault Char where
  parseDefault = asS $ \case
    [c] -> Right c
    _cs -> Left $ fromS "invalid Char"

instance ParseDefault Double where
  parseDefault = parseWithRead' "Double"

instance ParseDefault Float where
  parseDefault = parseWithRead' "Float"

instance ParseDefault Int where
  parseDefault = parseWithRead' "Int"

instance ParseDefault Int8 where
  parseDefault = parseWithRead' "Int8"

instance ParseDefault Int16 where
  parseDefault = parseWithRead' "Int16"

instance ParseDefault Int32 where
  parseDefault = parseWithRead' "Int32"

instance ParseDefault Int64 where
  parseDefault = parseWithRead' "Int64"

instance ParseDefault Integer where
  parseDefault = parseWithRead' "Integer"

instance ParseDefault Word where
  parseDefault = parseWithRead' "Word"

instance ParseDefault Word8 where
  parseDefault = parseWithRead' "Word8"

instance ParseDefault Word16 where
  parseDefault = parseWithRead' "Word16"

instance ParseDefault Word32 where
  parseDefault = parseWithRead' "Word32"

instance ParseDefault Word64 where
  parseDefault = parseWithRead' "Word64"

instance ParseDefault String where
  parseDefault = Right . toS

instance ParseDefault BSL.ByteString where
  parseDefault = Right . toBSL

instance ParseDefault BS.ByteString where
  parseDefault = Right . toBS

instance ParseDefault TL.Text where
  parseDefault = Right . toTL

instance ParseDefault T.Text where
  parseDefault = Right . toT

------------------------------------------------------------------------------
-- $ParseSpecific
--
-- These functions are equivalent to 'parse', but they specify the type being
-- parsed from.  Use them to avoid having to write type annotations in cases
-- where the type is ambiguous.

-- | Parse from a 'String'
--
-- @since 0.3.0.0
parseS :: (Parse a, Textual e) => String -> Either e a
parseS = parse
{-# INLINE parseS #-}

-- | Parse from strict 'T.Text'
--
-- @since 0.3.0.0
parseT :: (Parse a, Textual e) => T.Text -> Either e a
parseT = parse
{-# INLINE parseT #-}

-- | Parse from lazy 'TL.Text'
--
-- @since 0.3.0.0
parseTL :: (Parse a, Textual e) => TL.Text -> Either e a
parseTL = parse
{-# INLINE parseTL #-}

-- | Parse from a @Text@ 'TLB.Builder'
--
-- @since 1.1.0.0
parseTLB :: (Parse a, Textual e) => TLB.Builder -> Either e a
parseTLB = parse
{-# INLINE parseTLB #-}

-- | Parse from a strict 'BS.ByteString'
--
-- @since 0.3.0.0
parseBS :: (Parse a, Textual e) => BS.ByteString -> Either e a
parseBS = parse
{-# INLINE parseBS #-}

-- | Parse from a lazy 'BSL.ByteString'
--
-- @since 0.3.0.0
parseBSL :: (Parse a, Textual e) => BSL.ByteString -> Either e a
parseBSL = parse
{-# INLINE parseBSL #-}

-- | Parse from a @ByteString@ 'BSB.Builder'
--
-- @since 1.1.0.0
parseBSB :: (Parse a, Textual e) => BSB.Builder -> Either e a
parseBSB = parse
{-# INLINE parseBSB #-}

-- | Parse from a 'SBS.ShortByteString'
--
-- @since 1.1.0.0
parseSBS :: (Parse a, Textual e) => SBS.ShortByteString -> Either e a
parseSBS = parse
{-# INLINE parseSBS #-}

------------------------------------------------------------------------------
-- $ParseMaybe
--
-- The 'parseMaybe' function parses to a 'Maybe' type instead of an 'Either'
-- type.  The rest of the functions are equivalent to 'parseMaybe', but they
-- specify the type being parsed from.  Use them to avoid having to write type
-- annotations in cases where the type is ambiguous.

-- | Parse to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybe :: (Parse a, Textual t) => t -> Maybe a
parseMaybe = either (const Nothing) Just . parse'
{-# INLINE parseMaybe #-}

-- | Parse from a 'String' to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybeS :: Parse a => String -> Maybe a
parseMaybeS = parseMaybe
{-# INLINE parseMaybeS #-}

-- | Parse from strict 'T.Text' to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybeT :: Parse a => T.Text -> Maybe a
parseMaybeT = parseMaybe
{-# INLINE parseMaybeT #-}

-- | Parse from lazy 'TL.Text' to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybeTL :: Parse a => TL.Text -> Maybe a
parseMaybeTL = parseMaybe
{-# INLINE parseMaybeTL #-}

-- | Parse from a @Text@ 'TLB.Builder' to a 'Maybe' type
--
-- @since 1.1.0.0
parseMaybeTLB :: Parse a => TLB.Builder -> Maybe a
parseMaybeTLB = parseMaybe
{-# INLINE parseMaybeTLB #-}

-- | Parse from a strict 'BS.ByteString' to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybeBS :: Parse a => BS.ByteString -> Maybe a
parseMaybeBS = parseMaybe
{-# INLINE parseMaybeBS #-}

-- | Parse from a lazy 'BSL.ByteString' to a 'Maybe' type
--
-- @since 0.3.0.0
parseMaybeBSL :: Parse a => BSL.ByteString -> Maybe a
parseMaybeBSL = parseMaybe
{-# INLINE parseMaybeBSL #-}

-- | Parse from a @ByteString@ 'BSB.Builder' to a 'Maybe' type
--
-- @since 1.1.0.0
parseMaybeBSB :: Parse a => BSB.Builder -> Maybe a
parseMaybeBSB = parseMaybe
{-# INLINE parseMaybeBSB #-}

-- | Parse from a 'SBS.ShortByteString' to a 'Maybe' type
--
-- @since 1.1.0.0
parseMaybeSBS :: Parse a => SBS.ShortByteString -> Maybe a
parseMaybeSBS = parseMaybe
{-# INLINE parseMaybeSBS #-}

------------------------------------------------------------------------------
-- $ParseUnsafe
--
-- The 'parseUnsafe' function raises an exception on error instead of using an
-- 'Either' type.  It should only be used when an error is not possible.  The
-- rest of the functions are equivalent to 'parseUnsafe', but they specify the
-- type being parsed from.  Use them to avoid having to write type annotations
-- in cases where the type is ambiguous.

-- | Unsafely parse
--
-- @since 0.1.0.0
parseUnsafe :: (HasCallStack, Parse a, Textual t) => t -> a
parseUnsafe = either (error . ("parseUnsafe: " ++)) id . parse
{-# INLINE parseUnsafe #-}

-- | Unsafely parse to a 'String'
--
-- @since 0.1.0.0
parseUnsafeS :: (HasCallStack, Parse a) => String -> a
parseUnsafeS = parseUnsafe
{-# INLINE parseUnsafeS #-}

-- | Unsafely parse to strict 'T.Text'
--
-- @since 0.1.0.0
parseUnsafeT :: (HasCallStack, Parse a) => T.Text -> a
parseUnsafeT = parseUnsafe
{-# INLINE parseUnsafeT #-}

-- | Unsafely parse to lazy 'TL.Text'
--
-- @since 0.1.0.0
parseUnsafeTL :: (HasCallStack, Parse a) => TL.Text -> a
parseUnsafeTL = parseUnsafe
{-# INLINE parseUnsafeTL #-}

-- | Unsafely parse to a @Text@ 'TLB.Builder'
--
-- @since 1.1.0.0
parseUnsafeTLB :: (HasCallStack, Parse a) => TLB.Builder -> a
parseUnsafeTLB = parseUnsafe
{-# INLINE parseUnsafeTLB #-}

-- | Unsafely parse to a strict 'BS.ByteString'
--
-- @since 0.1.0.0
parseUnsafeBS :: (HasCallStack, Parse a) => BS.ByteString -> a
parseUnsafeBS = parseUnsafe
{-# INLINE parseUnsafeBS #-}

-- | Unsafely parse to a lazy 'BSL.ByteString'
--
-- @since 0.1.0.0
parseUnsafeBSL :: (HasCallStack, Parse a) => BSL.ByteString -> a
parseUnsafeBSL = parseUnsafe
{-# INLINE parseUnsafeBSL #-}

-- | Unsafely parse to a @ByteString@ 'BSB.Builder'
--
-- @since 1.1.0.0
parseUnsafeBSB :: (HasCallStack, Parse a) => BSB.Builder -> a
parseUnsafeBSB = parseUnsafe
{-# INLINE parseUnsafeBSB #-}

-- | Unsafely parse to a 'SBS.ShortByteString'
--
-- @since 1.1.0.0
parseUnsafeSBS :: (HasCallStack, Parse a) => SBS.ShortByteString -> a
parseUnsafeSBS = parseUnsafe
{-# INLINE parseUnsafeSBS #-}

------------------------------------------------------------------------------
-- $ParseWithASingleErrorMessage
--
-- The 'maybe' function takes an error message and a 'Maybe' value.  It
-- returns a 'Parse' result: the error when the 'Maybe' value is 'Nothing', or
-- the value inside the 'Just'.  This provides a convenient way to return the
-- same error message for any parse error.

-- | Create a 'Parse' result from a 'Textual' error message and a 'Maybe'
-- value
--
-- @since 1.2.0.0
maybe
  :: (Textual e', Textual e)
  => e'
  -> Maybe a
  -> Either e a
maybe err = Prelude.maybe (Left $ convert err) Right
{-# INLINE maybe #-}

-- | Create a 'Parse' result from a 'String' error message and a 'Maybe' value
--
-- @since 1.2.0.0
maybeS
  :: Textual e
  => String
  -> Maybe a
  -> Either e a
maybeS = maybe
{-# INLINE maybeS #-}

-- | Create a 'Parse' result from a 'T.Text' error message and a 'Maybe' value
--
-- @since 1.2.0.0
maybeT
  :: Textual e
  => T.Text
  -> Maybe a
  -> Either e a
maybeT = maybe
{-# INLINE maybeT #-}

-- | Create a 'Parse' result from a 'TL.Text' error message and a 'Maybe'
-- value
--
-- @since 1.2.0.0
maybeTL
  :: Textual e
  => TL.Text
  -> Maybe a
  -> Either e a
maybeTL = maybe
{-# INLINE maybeTL #-}

-- | Create a 'Parse' result from a 'TLB.Builder' error message and a 'Maybe'
-- value
--
-- @since 1.2.0.0
maybeTLB
  :: Textual e
  => TLB.Builder
  -> Maybe a
  -> Either e a
maybeTLB = maybe
{-# INLINE maybeTLB #-}

-- | Create a 'Parse' result from a 'BS.ByteString' error message and a
-- 'Maybe' value
--
-- @since 1.2.0.0
maybeBS
  :: Textual e
  => BS.ByteString
  -> Maybe a
  -> Either e a
maybeBS = maybe
{-# INLINE maybeBS #-}

-- | Create a 'Parse' result from a 'BSL.ByteString' error message and a
-- 'Maybe' value
--
-- @since 1.2.0.0
maybeBSL
  :: Textual e
  => BSL.ByteString
  -> Maybe a
  -> Either e a
maybeBSL = maybe
{-# INLINE maybeBSL #-}

-- | Create a 'Parse' result from a 'BSB.Builder' error message and a
-- 'Maybe' value
--
-- @since 1.2.0.0
maybeBSB
  :: Textual e
  => BSB.Builder
  -> Maybe a
  -> Either e a
maybeBSB = maybe
{-# INLINE maybeBSB #-}

-- | Create a 'Parse' result from a 'SBS.ShortByteString' error message and a
-- 'Maybe' value
--
-- @since 1.2.0.0
maybeSBS
  :: Textual e
  => SBS.ShortByteString
  -> Maybe a
  -> Either e a
maybeSBS = maybe
{-# INLINE maybeSBS #-}

------------------------------------------------------------------------------
-- $ParseEnums

-- | Parse a value in an enumeration
--
-- This function is intended to be used with types that have few choices, as
-- the implementation uses a linear algorithm.
--
-- See the @enum@ example program in the @examples@ directory.
--
-- @since 0.1.0.0
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
          _vs -> Left ambiguousError
  where
    norm :: T.Text -> T.Text
    norm = if allowCI then T.toLower else id

    match :: T.Text -> T.Text -> Bool
    match = if allowPrefix then T.isPrefixOf else (==)

-- | Parse a value in an enumeration, with 'Textual' error messages
--
-- The following English error messages are returned:
--
-- * \"invalid {name}\" when there are no matches
-- * \"ambiguous {name}\" when there is more than one match
--
-- @since 0.4.0.0
parseEnum'
  :: (Bounded a, Enum a, Render a, Textual t, Textual e)
  => String      -- ^ name to include in error messages
  -> Bool        -- ^ case-insensitive when 'True'
  -> Bool        -- ^ accept unique prefixes when 'True'
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseEnum' name allowCI allowPrefix =
    parseEnum
      allowCI allowPrefix
      (fromS $ "invalid " ++ name)
      (fromS $ "ambiguous " ++ name)
{-# INLINEABLE parseEnum' #-}

------------------------------------------------------------------------------
-- $ReadInstanced

-- | Parse a value using the 'Read' instance
--
-- @since 0.1.0.0
parseWithRead
  :: (Read a, Textual t)
  => e           -- ^ invalid input error
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseWithRead invalidError =
    Prelude.maybe (Left invalidError) Right . readMaybe . toS
{-# INLINEABLE parseWithRead #-}

-- | Parse a value using the 'Read' instance, with 'Textual' error messages
--
-- The following English error message is returned:
--
-- * \"invalid {name}\" when the parse fails
--
-- @since 0.3.0.0
parseWithRead'
  :: (Read a, Textual t, Textual e)
  => String      -- ^ name to include in error messages
  -> t           -- ^ textual input to parse
  -> Either e a  -- ^ error or parsed value
parseWithRead' name = parseWithRead (fromS $ "invalid " ++ name)
{-# INLINEABLE parseWithRead' #-}

-- | Parse a value to a 'Maybe' type using the 'Read' instance
--
-- @since 0.3.0.0
maybeParseWithRead
  :: (Read a, Textual t)
  => t        -- ^ textual input to parse
  -> Maybe a  -- ^ parsed value or 'Nothing' if invalid
maybeParseWithRead = readMaybe . toS

-- | Implement 'ReadS' using 'parseEnum'
--
-- This implementation expects all of the input to be consumed.
--
-- @since 0.1.0.0
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
--
-- @since 0.3.0.0
readsWithParse
  :: Parse a
  => ReadS a
readsWithParse s = case parseMaybe s of
    Just v  -> [(v, "")]
    Nothing -> []
{-# INLINEABLE readsWithParse #-}

------------------------------------------------------------------------------
-- $ParseValid
--
-- The follow functions provide a number of ways to use a 'Parse' instance to
-- validate constants at compile-time.
--
-- If you can use Template Haskell typed expressions in your project, use
-- 'valid', 'mkValid', or 'validOf'.  Use 'valid' to define constants for
-- types that have a 'THS.Lift' instance.  For types that do not have a
-- 'THS.Lift' instance, use 'mkValid' to define a validation function for that
-- type using a 'Proxy', or use 'validOf' to pass the 'Proxy' when defining
-- constants.
--
-- Typed expressions were not supported in @haskell-src-exts <1.22.0@, which
-- causes problems with old versions of @hlint@.  If the issue affects you,
-- you may use 'mkUntypedValid', 'mkUntypedValidQQ', or 'untypedValidOf'
-- instead of the above functions.  Use 'mkUntypedValid' to define a
-- validation function for a type using a 'Proxy', or use 'untypedValidOf' to
-- pass the 'Proxy' when defining constants.  Alternatively, use
-- 'mkUntypedValidQQ' to define a validation quasi-quoter.
--
-- For more details, see the following article:
-- <https://www.extrema.is/articles/ttc-textual-type-classes/validated-constants>

-- | Validate a constant at compile-time using a 'Parse' instance
--
-- This function parses the 'String' at compile-time and fails compilation on
-- error.  When valid, the result is compiled in, so the result type must have
-- a 'THS.Lift' instance.  When this is inconvenient, use one of the
-- alternative functions in this library.
--
-- This function uses a Template Haskell typed expression.  Typed expressions
-- were not supported in @haskell-src-exts <1.22.0@, which causes problems
-- with old versions of @hlint@.  If the issue affects you, use
-- @hlint -i "Parse error"@ to ignore parse errors or use one of the
-- alternative functions in this library.
--
-- Note that the typed Template Haskell API changed in GHC 9.  The type
-- displayed in this documentation is determined by the version of GHC used to
-- build the documentation.
--
-- The type of this function in GHC 9 or later is as follows:
--
-- @
-- valid
--   :: (MonadFail m, THS.Quote m, Parse a, THS.Lift a)
--   => String
--   -> THS.Code m a
-- @
--
-- The type of this function in previous versions of GHC is as follows:
--
-- @
-- valid
--   :: (Parse a, THS.Lift a)
--   => String
--   -> TH.Q (TH.TExp a)
-- @
--
-- This function is used the same way in all GHC versions.  See the @valid@,
-- @invalid@, and @lift@ example programs in the @examples@ directory.  The
-- following is example usage from the @valid@ example:
--
-- @
-- sample :: Username
-- sample = $$(TTC.valid "tcard")
-- @
--
-- @since 0.1.0.0
#if __GLASGOW_HASKELL__ >= 900
valid
  :: (MonadFail m, THS.Quote m, Parse a, THS.Lift a)
  => String
  -> THS.Code m a
valid s = case parse s of
    Right x -> [|| x ||]
    Left err -> THS.Code . fail $ "Invalid constant: " ++ err
#else
valid
  :: (Parse a, THS.Lift a)
  => String
  -> TH.Q (TH.TExp a)
valid s = case parse s of
    Right x -> [|| x ||]
    Left err -> fail $ "Invalid constant: " ++ err
#endif

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
-- This function uses a Template Haskell typed expression.  Typed expressions
-- were not supported in @haskell-src-exts <1.22.0@, which causes problems
-- with old versions of @hlint@.  If the issue affects you, use
-- @hlint -i "Parse error"@ to ignore parse errors or use 'untypedValidOf'
-- instead.
--
-- Note that the typed Template Haskell API changed in GHC 9.  The type
-- displayed in this documentation is determined by the version of GHC used to
-- build the documentation.
--
-- The type of this function in GHC 9 or later is as follows:
--
-- @
-- validOf
--   :: (MonadFail m, THS.Quote m, Parse a)
--   => Proxy a
--   -> String
--   -> THS.Code m a
-- @
--
-- The type of this function in previous versions of GHC is as follows:
--
-- @
-- validOf
--   :: Parse a
--   => Proxy a
--   -> String
--   -> TH.Q (TH.TExp a)
-- @
--
-- This function is used the same way in all GHC versions.  See the @validof@
-- example program in the @examples@ directory.  The following is example
-- usage from the @validof@ example:
--
-- @
-- sample :: Username
-- sample = $$(TTC.validOf (Proxy :: Proxy Username) "tcard")
-- @
--
-- @since 0.1.0.0
#if __GLASGOW_HASKELL__ >= 900
validOf
  :: (MonadFail m, THS.Quote m, Parse a)
  => Proxy a
  -> String
  -> THS.Code m a
validOf proxy s = case (`asProxyTypeOf` proxy) <$> parse s of
    Right{} -> [|| parseUnsafeS s ||]
    Left err -> THS.Code . fail $ "Invalid constant: " ++ err
#else
validOf
  :: Parse a
  => Proxy a
  -> String
  -> TH.Q (TH.TExp a)
validOf proxy s = case (`asProxyTypeOf` proxy) <$> parse s of
    Right{} -> [|| parseUnsafeS s ||]
    Left err -> fail $ "Invalid constant: " ++ err
#endif

-- | Make a @valid@ function using 'validOf' for the given type
--
-- Create a @valid@ function for a type in order to avoid having to write a
-- 'Proxy' when defining constants.
--
-- This function uses a Template Haskell typed expression.  Typed expressions
-- were not supported in @haskell-src-exts <1.22.0@, which causes problems
-- with old versions of @hlint@.  If the issue affects you, use
-- @hlint -i "Parse error"@ to ignore parse errors or use 'mkUntypedValid'
-- instead.
--
-- Note that the typed Template Haskell API changed in GHC 9.  The type
-- displayed in this documentation is determined by the version of GHC used to
-- build the documentation.
--
-- The type of the created @valid@ function in GHC 9 or later is as follows:
--
-- @
-- \$funName
--   :: forall m. (MonadFail m, THS.Quote m)
--   => String
--   -> THS.Code m $resultType
-- @
--
-- The type of the created @valid@ function in previous versions of GHC is as
-- follows:
--
-- @
-- \$funName
--   :: String
--   -> TH.Q (TH.TExp $resultType)
-- @
--
-- This function is used the same way in all GHC versions.  See the @mkvalid@
-- example program in the @examples@ directory.  The following is example
-- usage from the @mkvalid@ example:
--
-- @
-- \$(TTC.mkValid "valid" ''Username)
-- @
--
-- The created @valid@ function can then be used as follows:
--
-- @
-- sample :: Username
-- sample = $$(Username.valid "tcard")
-- @
--
-- @since 0.1.0.0
mkValid
  :: String
  -> TH.Name
  -> TH.DecsQ
mkValid funName typeName = do
    let funName' = TH.mkName funName
        resultType = pure $ TH.ConT typeName
#if __GLASGOW_HASKELL__ >= 900
    funType <-
      [t|
        forall m . (MonadFail m, THS.Quote m) =>
          String -> THS.Code m $resultType
        |]
#else
    funType <- [t| String -> TH.Q (TH.TExp $resultType) |]
#endif
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
-- See the @uvalidof@ example program in the @examples@ directory.  The
-- following is example usage from the @uvalidof@ example:
--
-- @
-- sample :: Username
-- sample = $(TTC.untypedValidOf (Proxy :: Proxy Username) "tcard")
-- @
--
-- @since 0.2.0.0
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
-- Create a @valid@ function for a type in order to avoid having to write a
-- 'Proxy' when defining constants.
--
-- See the @mkuvalid@ example program in the @examples@ directory.  The
-- following is example usage from the @mkuvalid@ example:
--
-- @
-- \$(TTC.mkUntypedValid "valid" ''Username)
-- @
--
-- The created @valid@ function can then be used as follows:
--
-- @
-- sample :: Username
-- sample = $(Username.valid "tcard")
-- @
--
-- @since 0.2.0.0
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
-- See the @uvalidqq@ example program in the @examples@ directory.  The
-- following is example usage from the @uvalidqq@ example:
--
-- @
-- \$(TTC.mkUntypedValidQQ "valid" ''Username)
-- @
--
-- The created @valid@ function can then be used as follows:
--
-- @
-- sample :: Username
-- sample = [Username.valid|tcard|]
-- @
--
-- @since 0.2.0.0
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
