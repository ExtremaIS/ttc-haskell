------------------------------------------------------------------------------
-- |
-- Module      : Username
-- Description : username data type
-- Copyright   : Copyright (c) 2019-2020 Travis Cardwell
-- License     : MIT
--
-- This module defines a username data type, with 'TTC.Render' and 'TTC.Parse'
-- instances, as an example of TTC usage.
--
-- The constructor for 'Username' is not exported.  The 'TTC.Parse' instance
-- serves as a "smart constructor," ensuring that all values are valid.
--
-- This version of the module provides a trivial example of deriving
-- 'THS.Lift'.
--
-- The 'Username' data type derives 'THS.Lift', using the @DeriveLift@
-- language extension.  The underlying 'String' already has a 'THS.Lift'
-- instance.
------------------------------------------------------------------------------

{-# LANGUAGE DeriveLift #-}

module Username (Username) where

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Char (isAsciiLower)

-- https://hackage.haskell.org/package/template-haskell
import qualified Language.Haskell.TH.Syntax as THS

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | A 'Username' must consist of 3 to 12 lowercase ASCII letters.
newtype Username = Username String
  deriving (Eq, Ord, Show, THS.Lift)

instance TTC.Render Username where
  render (Username s) = TTC.convert s

instance TTC.Parse Username where
  parse = TTC.asS $ \s -> do
    unless (all isAsciiLower s) $ Left "username has invalid character(s)"
    let len = length s
    when (len < 3) $ Left "username has fewer than 3 characters"
    when (len > 12) $ Left "username has more than 12 characters"
    pure $ Username s
