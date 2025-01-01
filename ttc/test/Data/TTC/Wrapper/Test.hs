{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TTC.Wrapper.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- https://hackage.haskell.org/package/text-short
import qualified Data.Text.Short as ST

-- (ttc)
import qualified Data.TTC as TTC
import qualified Data.TTC.Wrapper as TTCW

------------------------------------------------------------------------------

instance Eq BSB.Builder where
  x == y = BSB.toLazyByteString x == BSB.toLazyByteString y

#if !MIN_VERSION_bytestring(0,11,1)
instance Show BSB.Builder where
  show = show . BSB.toLazyByteString
#endif

------------------------------------------------------------------------------
-- $TestData

xS :: String
xS = "test テスト"

xT :: T.Text
xT = "test テスト"

------------------------------------------------------------------------------
-- $WrapperS

newtype ExS = ExS String
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperS
  deriving TTC.Render via TTCW.WrapperS

testWrapperS :: TestTree
testWrapperS = testCase "WrapperS" $
    Right xT @=? (TTC.render <$> TTC.parse @ExS @_ @String xT)

------------------------------------------------------------------------------
-- $WrapperT

newtype ExT = ExT T.Text
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperT
  deriving TTC.Render via TTCW.WrapperT

testWrapperT :: TestTree
testWrapperT = testCase "WrapperT" $
    Right xS @=? (TTC.render <$> TTC.parse @ExT @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperTL

newtype ExTL = ExTL TL.Text
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperTL
  deriving TTC.Render via TTCW.WrapperTL

testWrapperTL :: TestTree
testWrapperTL = testCase "WrapperTL" $
    Right xS @=? (TTC.render <$> TTC.parse @ExTL @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperTLB

newtype ExTLB = ExTLB TLB.Builder
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperTLB
  deriving TTC.Render via TTCW.WrapperTLB

testWrapperTLB :: TestTree
testWrapperTLB = testCase "WrapperTLB" $
    Right xS @=? (TTC.render <$> TTC.parse @ExTLB @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperST

newtype ExST = ExST ST.ShortText
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperST
  deriving TTC.Render via TTCW.WrapperST

testWrapperST :: TestTree
testWrapperST = testCase "WrapperST" $
    Right xS @=? (TTC.render <$> TTC.parse @ExST @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperBS

newtype ExBS = ExBS BS.ByteString
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperBS
  deriving TTC.Render via TTCW.WrapperBS

testWrapperBS :: TestTree
testWrapperBS = testCase "WrapperBS" $
    Right xS @=? (TTC.render <$> TTC.parse @ExBS @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperBSL

newtype ExBSL = ExBSL BSL.ByteString
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperBSL
  deriving TTC.Render via TTCW.WrapperBSL

testWrapperBSL :: TestTree
testWrapperBSL = testCase "WrapperBSL" $
    Right xS @=? (TTC.render <$> TTC.parse @ExBSL @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperBSB

newtype ExBSB = ExBSB BSB.Builder
  deriving (Eq, Show)
  deriving TTC.Parse via TTCW.WrapperBSB
  deriving TTC.Render via TTCW.WrapperBSB

testWrapperBSB :: TestTree
testWrapperBSB = testCase "WrapperBSB" $
    Right xS @=? (TTC.render <$> TTC.parse @ExBSB @_ @String xS)

------------------------------------------------------------------------------
-- $WrapperSBS

newtype ExSBS = ExSBS SBS.ShortByteString
  deriving (Eq, Ord, Show)
  deriving TTC.Parse via TTCW.WrapperSBS
  deriving TTC.Render via TTCW.WrapperSBS

testWrapperSBS :: TestTree
testWrapperSBS = testCase "WrapperSBS" $
    Right xS @=? (TTC.render <$> TTC.parse @ExSBS @_ @String xS)

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Data.TTC.Wrapper"
    [ testWrapperS
    , testWrapperT
    , testWrapperTL
    , testWrapperTLB
    , testWrapperST
    , testWrapperBS
    , testWrapperBSL
    , testWrapperBSB
    , testWrapperSBS
    ]
