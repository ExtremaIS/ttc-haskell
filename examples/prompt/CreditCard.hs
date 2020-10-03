------------------------------------------------------------------------------
-- |
-- Module      : CreditCard
-- Description : credit card data types
-- Copyright   : Copyright (c) 2019-2020 Travis Cardwell
-- License     : MIT
--
-- This module defines the data types for a credit card, with 'TTC.Render' and
-- 'TTC.Parse' instances, as an example of TTC usage.
--
-- The constructors and accessors of composite data types 'CreditCard' and
-- 'ExpirationDate' are exported, but the constructors for 'Name', 'Number',
-- 'Year', 'Month', and 'SecurityCode' are not.  The 'TTC.Parse' instances
-- serve as "smart constructors," ensuring that all values are valid.
------------------------------------------------------------------------------

module CreditCard
  ( -- * CreditCard
    CreditCard(..)
    -- ** Name
  , Name
    -- ** Number
  , Number
    -- ** ExpirationDate
  , ExpirationDate(..)
  , toDay
  , Year
  , Month
    -- ** SecurityCode
  , SecurityCode
  ) where

-- http://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit, isSpace, toUpper)
import Data.List (dropWhileEnd, intersperse)
import Text.Read (readMaybe)

-- http://hackage.haskell.org/package/time
import qualified Data.Time.Calendar as Calendar

-- (ttc)
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- | A credit card has a name, number, expiration date, and security code.
data CreditCard
  = CreditCard
    { name           :: !Name
    , number         :: !Number
    , expirationDate :: !ExpirationDate
    , securityCode   :: !SecurityCode
    }
  deriving Show

------------------------------------------------------------------------------

-- | After any leading and trailing whitespace is stripped and all lowercase
-- characters are converted to uppercase, a name must meet the following
-- constraints:
--
-- * Only characters between `0x20` (space) and `0x5F` (underscore) are
--   allowed.
-- * The name must be between 1 and 26 characters in length.
--
-- Reference:
--
-- * https://stackoverflow.com/questions/2004532
newtype Name = Name String
  deriving (Eq, Ord, Show)

instance TTC.Parse Name where
  parse = TTC.asS $ \s -> first TTC.fromS $ do
    let name' = map toUpper $ strip s
        invChars = filter ((||) <$> (< ' ') <*> (> '_')) name'
    unless (null invChars) . Left $
      "name has invalid character(s): " ++ intersperse ',' invChars
    when (null name') $ Left "name is empty"
    when (length name' > 26) $ Left "name has more than 26 characters"
    pure $ Name name'

instance TTC.Render Name where
  render (Name name') = TTC.convert name'

------------------------------------------------------------------------------

-- | After any space and dash characters are removed, a number must meet the
-- following constraints:
--
-- * Only ASCII digits are allowed.
-- * The number must be between 8 and 19 characters in length.
-- * The number must have a valid checksum.
--
-- Reference:
--
-- * https://en.wikipedia.org/wiki/Payment_card_number
-- * https://en.wikipedia.org/wiki/Luhn_algorithm
-- * http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Haskell
newtype Number = Number String
  deriving (Eq, Ord, Show)

instance TTC.Parse Number where
  parse = TTC.asS $ \s -> first TTC.fromS $ do
    let number' = filter ((&&) <$> (/= ' ') <*> (/= '-')) s
        invChars = filter (not . isDigit) number'
        len = length number'
    unless (null invChars) . Left $
      "number has invalid character(s): " ++ intersperse ',' invChars
    unless (len >= 8) $ Left "number has fewer than 8 characters"
    unless (len <= 19) $ Left "number has more than 19 characters"
    unless (luhn number') $ Left "number checksum is invalid"
    pure $ Number number'

instance TTC.Render Number where
  render (Number number') = TTC.convert number'

luhn :: String -> Bool
luhn
    = (== 0)
    . (`mod` 10)
    . sum
    . map (uncurry (+) . (`divMod` 10))
    . zipWith (*) (cycle [1, 2])
    . map digitToInt
    . reverse

------------------------------------------------------------------------------

-- | When parsed from a single string, an expiration date must be in `YYYY-MM`
-- format.
data ExpirationDate
  = ExpirationDate
    { year  :: !Year
    , month :: !Month
    }
  deriving (Eq, Ord, Show)

instance TTC.Parse ExpirationDate where
  parse = TTC.asS $ \s -> case break (== '-') (strip s) of
    (year', '-':month') ->
      ExpirationDate <$> TTC.parse year' <*> TTC.parse month'
    _ -> Left $ TTC.fromS "expiration date not in YYYY-MM format"

instance TTC.Render ExpirationDate where
  render (ExpirationDate year' month') =
    TTC.fromS $ TTC.render year' ++ "-" ++ TTC.render month'

toDay
  :: ExpirationDate
  -> Calendar.Day
toDay (ExpirationDate (Year year') (Month month')) =
    let yearZ = fromIntegral year'
        day = Calendar.gregorianMonthLength yearZ month'
    in Calendar.fromGregorian yearZ month' day

------------------------------------------------------------------------------

-- | A year must be in `YYYY` format, between 1900 and 9999.
newtype Year = Year Int
  deriving (Eq, Ord, Show)

instance TTC.Parse Year where
  parse = TTC.asS $ \s -> first TTC.fromS $ do
    year' <- maybe (Left "year is not in YYYY format") pure $ readMaybe s
    unless (year' >= 1900) $ Left "year is before 1900"
    unless (year' <= 9999) $ Left "year is after 9999"
    pure $ Year year'

instance TTC.Render Year where
  render (Year year') = TTC.convert $ show year'

------------------------------------------------------------------------------

-- | A month must be in `MM` format, between 1 (January) and 12 (December).
newtype Month = Month Int
  deriving (Eq, Ord, Show)

instance TTC.Parse Month where
  parse = TTC.asS $ \s -> first TTC.fromS $ do
    month' <- maybe (Left "month is not in MM format") pure $ readMaybe s
    unless (month' >= 1 && month' <= 12) $ Left "month is not in 1-12 range"
    pure $ Month month'

instance TTC.Render Month where
  render (Month month')
    | month' < 10 = TTC.convert $ '0' : show month'
    | otherwise   = TTC.convert $ show month'

------------------------------------------------------------------------------

-- | After any leading and trailing whitespace is stripped, a security code
-- must meet the following constraints:
--
-- * Only ASCII digits are allowed.
-- * The number must be 3 or 4 characters in length.
--
-- Reference:
--
-- * https://en.wikipedia.org/wiki/Card_security_code
newtype SecurityCode = SecurityCode String
  deriving (Eq, Ord, Show)

instance TTC.Parse SecurityCode where
  parse = TTC.asS $ \s -> first TTC.fromS $ do
    let securityCode' = strip s
        invChars = filter (not . isDigit) securityCode'
        len = length securityCode'
    unless (null invChars) . Left $
      "security code has invalid character(s): " ++ intersperse ',' invChars
    unless (len >= 3) $ Left "security code has fewer than 3 characters"
    unless (len <= 4) $ Left "security code has more than 4 characters"
    pure $ SecurityCode securityCode'

instance TTC.Render SecurityCode where
  render (SecurityCode securityCode') = TTC.convert securityCode'

------------------------------------------------------------------------------

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
