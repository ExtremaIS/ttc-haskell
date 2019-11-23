------------------------------------------------------------------------------
-- |
-- Module      : Duration
-- Description : duration data type
-- Copyright   : Copyright (c) 2019 Travis Cardwell
-- License     : MIT
--
-- This module defines a duration data type, with 'TTC.Render' and 'TTC.Parse'
-- instances, as an example of TTC usage.
--
-- The constructor for 'Duration' is not exported.  The 'TTC.Parse' instance
-- serves as a "smart constructor," ensuring that all values are valid.
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Duration
  ( -- * Type
    Duration
    -- * API
  , fromDouble
  , fromFloat
  , fromInt
  , fromMicro
  , fromNominalDiffTime
  , fromReal
  , toDouble
  , toFloat
  , toInt
  , toIntegral
  , toMicro
  , toNominalDiffTime
  , valid
  ) where

-- https://hackage.haskell.org/package/base
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/time
import Data.Time.Clock (NominalDiffTime)

-- ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------
-- $Type

-- | A 'Duration' is a measure of seconds.  It is fractional and must be zero
-- or greater.
--
-- The underlying type is 'NominalDiffTime', which may be negative.  The
-- constructor is not exported, and all functions that create a value ensure
-- that the constraint holds.
newtype Duration = Duration NominalDiffTime
  deriving Show

instance TTC.Parse Duration where
  parse = TTC.asS $ \s ->
    maybe (Left $ "invalid Duration: " <> s) fromDouble $ readMaybe s

instance TTC.Render Duration where
  render (Duration t) = TTC.fromS . dropWhileEnd (== 's') $ show t

------------------------------------------------------------------------------
-- $API

-- | Construct a 'Duration' from a 'Double' (seconds).
fromDouble :: Double -> Either String Duration
fromDouble = fromReal

-- | Construct a 'Duration' from a 'Float' (seconds).
fromFloat :: Float -> Either String Duration
fromFloat = fromReal

-- | Construct a 'Duration' from an 'Int' (seconds).
fromInt :: Int -> Either String Duration
fromInt = fromReal

-- | Construct a 'Duration' from an 'Int' (microseconds).
fromMicro :: Int -> Either String Duration
fromMicro us = fromDouble $ fromIntegral us / 1000000.0

-- | Construct a 'Duration' from a 'NominalDiffTime'.
fromNominalDiffTime :: NominalDiffTime -> Either String Duration
fromNominalDiffTime t
    | t >= 0.0 = Right $ Duration t
    | otherwise = Left $ "Duration less than zero: " ++ show t

-- | Construct a 'Duration' from a 'Real' (seconds).
fromReal :: (Real a, Show a) => a -> Either String Duration
fromReal x
    | x >= 0 = Right . Duration $ realToFrac x
    | otherwise = Left $ "Duration less than zero: " ++ show x

-- | Convert a 'Duration' to a 'Double' (seconds).
toDouble :: Duration -> Double
toDouble = toFractional

-- | Convert a 'Duration' to a 'Float' (seconds).
toFloat :: Duration -> Float
toFloat = toFractional

-- | Convert a 'Duration' to a 'Fractional' (seconds).
toFractional :: Fractional a => Duration -> a
toFractional (Duration t) = realToFrac t

-- | Convert a 'Duration' to an 'Int' (seconds).
toInt :: Duration -> Int
toInt = toIntegral

-- | Convert a 'Duration' to an 'Integral' (seconds).
toIntegral :: Integral a => Duration -> a
toIntegral = round . toFloat

-- | Convert a 'Duration' to an 'Int' (microseconds).
toMicro :: Duration -> Int
toMicro (Duration t) = round (realToFrac t * 1000000.0 :: Double)

-- | Convert a 'Duration' to a 'NominalDiffTime'.
toNominalDiffTime :: Duration -> NominalDiffTime
toNominalDiffTime (Duration t) = t

-- | The 'valid' function is created using 'TTC.mkValid' via Template Haskell.
-- Note that 'Duration' does not have a 'Language.Haskell.TH.Syntax.Lift'
-- instance.
$(TTC.mkValid "valid" ''Duration)
