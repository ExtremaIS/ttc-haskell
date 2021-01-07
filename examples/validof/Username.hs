------------------------------------------------------------------------------
-- |
-- Module      : Username
-- Description : username data type
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- This module defines a username data type, with 'TTC.Render' and 'TTC.Parse'
-- instances, as an example of TTC usage.
--
-- The constructor for 'Username' is not exported.  The 'TTC.Parse' instance
-- serves as a "smart constructor," ensuring that all values are valid.
------------------------------------------------------------------------------

module Username (Username) where

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | A 'Username' must consist of 3 to 12 lowercase ASCII letters.
newtype Username = Username Text
  deriving (Eq, Ord, Show)

instance TTC.Render Username where
  render (Username t) = TTC.convert t

instance TTC.Parse Username where
  parse = TTC.asT $ \t -> first TTC.fromS $ do
    unless (T.all isAsciiLower t) $ Left "username has invalid character(s)"
    let len = T.length t
    when (len < 3) $ Left "username has fewer than 3 characters"
    when (len > 12) $ Left "username has more than 12 characters"
    pure $ Username t
