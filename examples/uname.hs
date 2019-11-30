------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : minimal example of using Render and Parse instances
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Main (main) where

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:example-uname)
import Username (Username)

------------------------------------------------------------------------------

main :: IO ()
main = case TTC.parse "tcard" :: Either String Username of
    Right uname -> putStrLn $ "valid username: " ++ TTC.render uname
    Left err -> putStrLn $ "invalid username: " ++ err
