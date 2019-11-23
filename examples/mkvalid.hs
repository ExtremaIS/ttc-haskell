------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of compile-time validation
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
--
-- 'Data.TTC.mkValid' is used to create the 'Duration.valid' function, without
-- the need for a 'Language.Haskell.TH.Syntax.Lift' instance.  In this
-- example, the sample duration is validated at compile-time and parsed again
-- at run-time.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- (ttc:example-mkvalid)
import qualified Duration as Duration
import Duration (Duration)

-- HLint does not support typed expression splices
{-# ANN module "HLint: ignore" #-}

------------------------------------------------------------------------------

sample :: Duration
sample = $$(Duration.valid "123")

------------------------------------------------------------------------------

main :: IO ()
main = print sample
