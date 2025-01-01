------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : minimal example of using a wrapper type
-- Copyright   : Copyright (c) 2019-2025 Travis Cardwell
-- License     : MIT
--
-- This example demonstrates use of @DerivingVia@ to define TTC instances via
-- a wrapper type.
------------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}

module Main (main) where

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC
import qualified Data.TTC.Wrapper as TTCW

------------------------------------------------------------------------------

newtype Username = Username { usernameText :: Text }
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperT
  deriving TTC.Render via TTCW.WrapperT

------------------------------------------------------------------------------

testParse :: String -> IO ()
testParse s = do
    putStrLn $ "testParse " ++ show s
    putStrLn . (' ' :) $ case TTC.parse s :: Either String Username of
      Right uname -> "valid username: " ++ TTC.render uname
      Left err    -> err

------------------------------------------------------------------------------

main :: IO ()
main = do
    testParse "tcard"
    testParse "Travis"
