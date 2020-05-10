------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation
-- Copyright   : Copyright (c) 2019-2020 Travis Cardwell
-- License     : MIT
--
-- 'TTC.mkValid' is used to create @valid@ function, to validate constants of
-- a specific type.  The sample username is validated at compile-time.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- (ttc-examples:example-valid)
import qualified Username
import Username (Username)

-- HLint does not support typed expression splices
{-# ANN module "HLint: ignore" #-}

------------------------------------------------------------------------------

sample :: Username
sample = $$(Username.valid "tcard")

------------------------------------------------------------------------------

main :: IO ()
main = print sample
