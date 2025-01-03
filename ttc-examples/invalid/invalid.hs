------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation error
-- Copyright   : Copyright (c) 2019-2025 Travis Cardwell
-- License     : MIT
--
-- 'TTC.valid' is used to create validated constants.  The sample username
-- is (in)validated at compile-time.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- (ttc-examples:ttc-example-invalid)
import Username (Username)

------------------------------------------------------------------------------

sample :: Username
sample = $$(TTC.valid "bad-username")

------------------------------------------------------------------------------

main :: IO ()
main = print sample
