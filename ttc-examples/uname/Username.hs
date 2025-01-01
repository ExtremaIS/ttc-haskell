------------------------------------------------------------------------------
-- |
-- Module      : Username
-- Description : username data type
-- Copyright   : Copyright (c) 2019-2025 Travis Cardwell
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
import Data.Char (isAsciiLower)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | A 'Username' must consist of 3 to 12 lowercase ASCII letters.
newtype Username = Username { usernameText :: Text }
  deriving (Eq, Ord, Show)

instance TTC.Parse Username where
  parse = TTC.asT $ \t -> TTC.prefixErrorS "invalid username: " $ do
    unless (T.all isAsciiLower t) $ Left "not only lowercase ASCII letters"
    let len = T.length t
    when (len < 3) $ Left "fewer than 3 characters"
    when (len > 12) $ Left "more than 12 characters"
    pure $ Username t

instance TTC.Render Username where
  render = TTC.convert . usernameText
