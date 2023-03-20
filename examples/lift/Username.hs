------------------------------------------------------------------------------
-- |
-- Module      : Username
-- Description : username data type
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
--
-- This module defines a username data type, with 'TTC.Render' and 'TTC.Parse'
-- instances, as an example of TTC usage.
--
-- The constructor for 'Username' is not exported.  The 'TTC.Parse' instance
-- serves as a "smart constructor," ensuring that all values are valid.
--
-- This version of the module provides an example of a non-derivived
-- 'THS.Lift' instance.  It uses 'Text', which does not have a 'THS.Lift'
-- instance until version @text-1.2.4.0@.
--
-- The 'Username' data type derives 'THS.Lift', using the @DeriveLift@
-- language extension.  The underlying 'Text' type has an instance since
-- version @text-1.2.4.0@ and needs to be provided an instance otherwise.
--
-- The instance unpacks the 'Text' and stores an expression to pack the
-- resulting 'String'.  Template Haskell is needed to splice in the
-- application of 'T.pack'.  The @TemplateHaskellQuotes@ language extension,
-- available from GHC 8, is sufficient, and the @TemplateHaskell@ language
-- extension is required in older versions.
--
-- Since the instance is an orphan, an @OPTIONS_GHC@ pragma is used to hide
-- the compilation warning.
--
-- The @CPP@ language extension is used to make the code compatible with a
-- wide range of GHC and library versions.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}

#if !MIN_VERSION_text(1,2,4)
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module Username (Username) where

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower)

-- https://hackage.haskell.org/package/template-haskell
#if !MIN_VERSION_text(1,2,4)
import qualified Language.Haskell.TH as TH
#endif
import qualified Language.Haskell.TH.Syntax as THS

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | A 'Username' must consist of 3 to 12 lowercase ASCII letters.
newtype Username = Username Text
  deriving (Eq, Ord, Show, THS.Lift)

instance TTC.Render Username where
  render (Username t) = TTC.convert t

instance TTC.Parse Username where
  parse = TTC.asT $ \t -> first TTC.fromS $ do
    unless (T.all isAsciiLower t) $ Left "username has invalid character(s)"
    let len = T.length t
    when (len < 3) $ Left "username has fewer than 3 characters"
    when (len > 12) $ Left "username has more than 12 characters"
    pure $ Username t

------------------------------------------------------------------------------

#if !MIN_VERSION_text(1,2,4)
instance THS.Lift Text where
  lift = TH.appE (TH.varE 'T.pack) . TH.stringE . T.unpack
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped =
    fmap THS.TExp . TH.appE (TH.varE 'T.pack) . TH.stringE . T.unpack
#endif
#endif
