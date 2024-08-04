------------------------------------------------------------------------------
-- |
-- Module      : Data.TTC.Wrapper
-- Description : TTC wrapper types
-- Copyright   : Copyright (c) 2019-2024 Travis Cardwell
-- License     : MIT
--
-- This module defines a @newtype@ wrapper for each 'TTC.Textual' data type.
-- Each wrapper has a 'TTC.Render' instance that converts the wrapped type and
-- a 'TTC.Parse' instance that wraps the argument (never failing).  Data types
-- that are coercible to a 'TTC.Textual' data type can use these wrappers to
-- derive instances using @DerivingVia@.
--
-- See the @wrapper@ example program in the @ttc-examples@ directory of the
-- source repository.
------------------------------------------------------------------------------

module Data.TTC.Wrapper
  ( -- * Wrapper Types
    WrapperS(..)
  , WrapperT(..)
  , WrapperTL(..)
  , WrapperTLB(..)
  , WrapperST(..)
  , WrapperBS(..)
  , WrapperBSL(..)
  , WrapperBSB(..)
  , WrapperSBS(..)
  ) where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- https://hackage.haskell.org/package/text-short
import qualified Data.Text.Short as ST

-- (ttc)
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | 'String' wrapper type
--
-- @since 1.5.0.0
newtype WrapperS = WrapperS { unWrapperS :: String }

instance TTC.Parse WrapperS where
  parse = TTC.asS $ pure . WrapperS

instance TTC.Render WrapperS where
  render = TTC.fromS . unWrapperS

------------------------------------------------------------------------------

-- | Strict 'T.Text' wrapper type
--
-- @since 1.5.0.0
newtype WrapperT = WrapperT { unWrapperT :: T.Text }

instance TTC.Parse WrapperT where
  parse = TTC.asT $ pure . WrapperT

instance TTC.Render WrapperT where
  render = TTC.fromT . unWrapperT

------------------------------------------------------------------------------

-- | Lazy 'TL.Text' wrapper type
--
-- @since 1.5.0.0
newtype WrapperTL = WrapperTL { unWrapperTL :: TL.Text }

instance TTC.Parse WrapperTL where
  parse = TTC.asTL $ pure . WrapperTL

instance TTC.Render WrapperTL where
  render = TTC.fromTL . unWrapperTL

------------------------------------------------------------------------------

-- | Text 'TLB.Builder' wrapper type
--
-- @since 1.5.0.0
newtype WrapperTLB = WrapperTLB { unWrapperTLB :: TLB.Builder }

instance TTC.Parse WrapperTLB where
  parse = TTC.asTLB $ pure . WrapperTLB

instance TTC.Render WrapperTLB where
  render = TTC.fromTLB . unWrapperTLB

------------------------------------------------------------------------------

-- | 'ST.ShortText' wrapper type
--
-- @since 1.5.0.0
newtype WrapperST = WrapperST { unWrapperST :: ST.ShortText }

instance TTC.Parse WrapperST where
  parse = TTC.asST $ pure . WrapperST

instance TTC.Render WrapperST where
  render = TTC.fromST . unWrapperST

------------------------------------------------------------------------------

-- | Strict 'BS.ByteString' wrapper type
--
-- @since 1.5.0.0
newtype WrapperBS = WrapperBS { unWrapperBS :: BS.ByteString }

instance TTC.Parse WrapperBS where
  parse = TTC.asBS $ pure . WrapperBS

instance TTC.Render WrapperBS where
  render = TTC.fromBS . unWrapperBS

------------------------------------------------------------------------------

-- | Lazy 'BSL.ByteString' wrapper type
--
-- @since 1.5.0.0
newtype WrapperBSL = WrapperBSL { unWrapperBSL :: BSL.ByteString }

instance TTC.Parse WrapperBSL where
  parse = TTC.asBSL $ pure . WrapperBSL

instance TTC.Render WrapperBSL where
  render = TTC.fromBSL . unWrapperBSL

------------------------------------------------------------------------------

-- | ByteString 'BSB.Builder' wrapper type
--
-- @since 1.5.0.0
newtype WrapperBSB = WrapperBSB { unWrapperBSB :: BSB.Builder }

instance TTC.Parse WrapperBSB where
  parse = TTC.asBSB $ pure . WrapperBSB

instance TTC.Render WrapperBSB where
  render = TTC.fromBSB . unWrapperBSB

------------------------------------------------------------------------------

-- | 'SBS.ShortByteString' wrapper type
--
-- @since 1.5.0.0
newtype WrapperSBS = WrapperSBS { unWrapperSBS :: SBS.ShortByteString }

instance TTC.Parse WrapperSBS where
  parse = TTC.asSBS $ pure . WrapperSBS

instance TTC.Render WrapperSBS where
  render = TTC.fromSBS . unWrapperSBS
