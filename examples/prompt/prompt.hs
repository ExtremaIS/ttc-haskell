------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example CLI prompt using TTC
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
--
-- 'TTC.Parse' and 'TTC.Render' instances are used to create a CLI prompt,
-- demonstrating TTC usage.
------------------------------------------------------------------------------

module Main (main) where

-- https://hackage.haskell.org/package/base
import qualified System.IO as IO

-- https://hackage.haskell.org/package/time
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:example-prompt)
import qualified CreditCard as CC
import CreditCard (CreditCard(CreditCard), ExpirationDate(ExpirationDate))

------------------------------------------------------------------------------

-- | This function prompts for a value of the desired type.  When input is
-- invalid, it displays the error and tries again.
prompt
  :: (String -> Either String a)  -- ^ parse function
  -> String                       -- ^ prompt string
  -> IO a
prompt parse promptString = loop
  where
    loop = do
      putStr promptString
      IO.hFlush IO.stdout
      s <- getLine
      case parse s of
        Right x -> return x
        Left err -> do
          putStrLn $ "  " <> err
          loop

-- | This is a version of 'prompt' that uses 'TTC.Parse' instances.
promptTTC
  :: TTC.Parse a
  => String  -- ^ prompt string
  -> IO a
promptTTC = prompt TTC.parse

------------------------------------------------------------------------------

-- | This function prompts for credit card details.
promptCC :: IO CreditCard
promptCC = CreditCard
    <$> promptTTC "Enter the name: "
    <*> promptTTC "Enter the number: "
    <*>
      ( ExpirationDate
          <$> promptTTC "Enter the expiration year (YYYY): "
          <*> promptTTC "Enter the expiration month (MM): "
      )
    <*> promptTTC "Enter the security code: "

------------------------------------------------------------------------------

-- | The program prompts for credit card details, prints out the normalized
-- values, and shows information about expiration.
main :: IO ()
main = do
    putStrLn "Please enter some fake credit card details."
    cc <- promptCC
    putStrLn $ replicate 78 '-'
    putStrLn $ "Name:            " <> TTC.render (CC.name cc)
    putStrLn $ "Number:          " <> TTC.render (CC.number cc)
    putStrLn $ "Expiration date: " <> TTC.render (CC.expirationDate cc)
    putStrLn $ "Security code:   " <> TTC.render (CC.securityCode cc)
    putStrLn $ replicate 78 '-'
    today <- Clock.utctDay <$> Clock.getCurrentTime
    putStrLn . ("This credit card " <>) . (<> "!") $
      case CC.toDay (CC.expirationDate cc) of
        expiry
          | expiry > today -> "expires in " <> diffDays expiry today
          | expiry < today -> "expired " <> diffDays today expiry <> " ago"
          | otherwise      -> "expires today"
  where
    diffDays :: Calendar.Day -> Calendar.Day -> String
    diffDays day1 day2 = case Calendar.diffDays day1 day2 of
      1 -> "1 day"
      n -> show n <> " days"
