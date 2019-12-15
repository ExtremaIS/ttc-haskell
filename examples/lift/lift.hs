------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
--
-- 'TTC.valid' is used to create validated constants.  The sample username
-- is validated at compile-time.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- (ttc-examples:example-valid)
import Username (Username)

-- HLint does not support typed expression splices
{-# ANN module "HLint: ignore" #-}

------------------------------------------------------------------------------

sample :: Username
sample = $$(TTC.valid "tcard")

------------------------------------------------------------------------------

main :: IO ()
main = print sample
