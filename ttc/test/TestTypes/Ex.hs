{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module TestTypes.Ex where

-- https://hackage.haskell.org/package/template-haskell
import qualified Language.Haskell.TH.Syntax as THS

-- (ttc)
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Type

newtype Ex = Ex { exString :: String }
  deriving (Eq, Ord, Show, THS.Lift)

instance TTC.Parse Ex where
  parse = TTC.asS $ pure . Ex

instance TTC.Render Ex where
  render = TTC.convert . exString

------------------------------------------------------------------------------
-- $API

$(TTC.mkValid "valid" ''Ex)

$(TTC.mkUntypedValid "untypedValid" ''Ex)

$(TTC.mkUntypedValidQQ "untypedValidQQ" ''Ex)
