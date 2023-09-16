------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
--
-- 'TTC.valid' is used to create validated constants.  The type must have a
-- `Lift` instance since this is implemented using typed Template Haskell.
-- When using the `OverloadedStrings` extension, you do not even have to write
-- the function name, at the expense of documentation.  With GHC 9, you can
-- even leave off the parenthesis.  The sample usernames are validated at
-- compile-time.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- (ttc-examples:example-valid)
import Username (Username)

------------------------------------------------------------------------------

sample1 :: Username
-- This syntax works with all supported versions of GHC.
sample1 = $$(TTC.valid "tcard")

sample2 :: Username
#if __GLASGOW_HASKELL__ >= 900
-- This syntax only works with GHC 9.
sample2 = $$"alice"
#else
-- This syntax works with all supported versions of GHC.
sample2 = $$("alice")
#endif

------------------------------------------------------------------------------

main :: IO ()
main = do
    print sample1
    print sample2
