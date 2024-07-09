{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module TestString where

-- https://hackage.haskell.org/package/template-haskell
import qualified Language.Haskell.TH.Syntax as THS

-- (ttc)
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Type

newtype TestString = TestString String
  deriving (Eq, Ord, Show, THS.Lift)

instance TTC.Render TestString where
  render (TestString s) = TTC.convert s

instance TTC.Parse TestString where
  parse = TTC.asS $ Right . TestString

------------------------------------------------------------------------------
-- $API

$(TTC.mkValid "valid" ''TestString)

$(TTC.mkUntypedValid "untypedValid" ''TestString)

$(TTC.mkUntypedValidQQ "untypedValidQQ" ''TestString)
