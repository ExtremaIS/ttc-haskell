------------------------------------------------------------------------------
-- |
-- Module      : Data.TTC.Instances
-- Description : instances for basic data types
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- This module defines TTC 'TTC.Render' and 'TTC.Parse' instances for some
-- basic data types.  The definitions for the numeric data types are
-- implemented using the 'Show' and 'Read' instances.  The definitions for the
-- character and textual data types are implemented without quoting.
--
-- To use these instances, explicitly import them as follows:
--
-- @
-- import Data.TTC.Instances ()
-- @
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TTC.Instances () where

-- https://hackage.haskell.org/package/base
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- (ttc)
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

instance TTC.Parse Char where
  parse = TTC.asS $ \case
    [c] -> Right c
    _cs -> Left $ TTC.fromS "invalid Char"

instance TTC.Render Char where
  render c = TTC.fromS [c]

------------------------------------------------------------------------------

instance TTC.Parse Double where
  parse = TTC.parseWithRead' "Double"

instance TTC.Render Double where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Float where
  parse = TTC.parseWithRead' "Float"

instance TTC.Render Float where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Int where
  parse = TTC.parseWithRead' "Int"

instance TTC.Render Int where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Int8 where
  parse = TTC.parseWithRead' "Int8"

instance TTC.Render Int8 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Int16 where
  parse = TTC.parseWithRead' "Int16"

instance TTC.Render Int16 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Int32 where
  parse = TTC.parseWithRead' "Int32"

instance TTC.Render Int32 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Int64 where
  parse = TTC.parseWithRead' "Int64"

instance TTC.Render Int64 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Integer where
  parse = TTC.parseWithRead' "Integer"

instance TTC.Render Integer where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Word where
  parse = TTC.parseWithRead' "Word"

instance TTC.Render Word where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Word8 where
  parse = TTC.parseWithRead' "Word8"

instance TTC.Render Word8 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Word16 where
  parse = TTC.parseWithRead' "Word16"

instance TTC.Render Word16 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Word32 where
  parse = TTC.parseWithRead' "Word32"

instance TTC.Render Word32 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse Word64 where
  parse = TTC.parseWithRead' "Word64"

instance TTC.Render Word64 where
  render = TTC.renderWithShow

------------------------------------------------------------------------------

instance TTC.Parse String where
  parse = Right . TTC.toS

instance TTC.Render String where
  render = TTC.fromS

------------------------------------------------------------------------------

instance TTC.Parse BSL.ByteString where
  parse = Right . TTC.toBSL

instance TTC.Render BSL.ByteString where
  render = TTC.fromBSL

------------------------------------------------------------------------------

instance TTC.Parse BS.ByteString where
  parse = Right . TTC.toBS

instance TTC.Render BS.ByteString where
  render = TTC.fromBS

------------------------------------------------------------------------------

instance TTC.Parse TL.Text where
  parse = Right . TTC.toTL

instance TTC.Render TL.Text where
  render = TTC.fromTL

------------------------------------------------------------------------------

instance TTC.Parse T.Text where
  parse = Right . TTC.toT

instance TTC.Render T.Text where
  render = TTC.fromT
