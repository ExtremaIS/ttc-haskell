------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation failure
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
--
-- 'TTC.valid' is used to create validated constants.  The credit card number
-- has an invalid checksum, triggering a compile-time error.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:example-invalid)
import CreditCard (CreditCard(CreditCard))

-- HLint does not support typed expression splices
{-# ANN module "HLint: ignore" #-}

------------------------------------------------------------------------------

sample :: CreditCard
sample = CreditCard
    $$(TTC.valid "John Q Doe")
    $$(TTC.valid "1234 5678 9012")
    $$(TTC.valid "2020-06")
    $$(TTC.valid "123")

------------------------------------------------------------------------------

main :: IO ()
main = print sample
