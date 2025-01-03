------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation
-- Copyright   : Copyright (c) 2019-2025 Travis Cardwell
-- License     : MIT
--
-- 'TTC.mkUntypedValidQQ' is used to create @valid@ quasi-quoter, to validate
-- constants of a specific type.  The sample username is validated at
-- compile-time.
------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

-- (ttc-examples:ttc-example-mkuvalidqq)
import qualified Username
import Username (Username)

------------------------------------------------------------------------------

sample :: Username
sample = [Username.valid|tcard|]

------------------------------------------------------------------------------

main :: IO ()
main = print sample
