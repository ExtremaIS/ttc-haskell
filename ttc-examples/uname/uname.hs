------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : minimal example of using Render and Parse instances
-- Copyright   : Copyright (c) 2019-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Main (main) where

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:example-uname)
import Username (Username)

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
