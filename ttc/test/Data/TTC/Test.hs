{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TTC.Test (tests) where

-- https://hackage.haskell.org/package/base
import Control.Exception (ErrorCall, Exception, evaluate, handle)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Stack (HasCallStack)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), Assertion, assertFailure, testCase)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

-- https://hackage.haskell.org/package/text-short
import qualified Data.Text.Short as ST

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:test)
import qualified TestString
import TestString (TestString(TestString))

------------------------------------------------------------------------------
-- $Instances

TTC.defaultRenderAndParseInstances
  [ ''Bool, ''Char, ''Double, ''Float, ''Integer
  , ''Int, ''Int8, ''Int16, ''Int32, ''Int64
  , ''Word, ''Word8, ''Word16, ''Word32, ''Word64
  , ''String
  , ''T.Text, ''TL.Text, ''TLB.Builder, ''ST.ShortText
  , ''BS.ByteString, ''BSL.ByteString, ''BSB.Builder, ''SBS.ShortByteString
  ]

------------------------------------------------------------------------------

instance Eq BSB.Builder where
  x == y = BSB.toLazyByteString x == BSB.toLazyByteString y

#if !MIN_VERSION_bytestring(0,11,1)
instance Show BSB.Builder where
  show = show . BSB.toLazyByteString
#endif

------------------------------------------------------------------------------
-- $HelperFunctions

assertRaises
  :: (HasCallStack, Exception e, Show a)
  => Proxy e
  -> a
  -> Assertion
assertRaises proxy x =
    handle
      (const (return ()) . (`asProxyTypeOf` proxy))
      (assertFailure . ("expected exception; got: " ++) . show =<< evaluate x)

------------------------------------------------------------------------------
-- $TestData

xS :: String
xS = "test テスト"

xT :: T.Text
xT = "test テスト"

xTL :: TL.Text
xTL = "test テスト"

xTLB :: TLB.Builder
xTLB = "test テスト"

xST :: ST.ShortText
xST = "test テスト"

xBS :: BS.ByteString
xBS = "test \xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88"

xBSL :: BSL.ByteString
xBSL = "test \xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88"

xBSB :: BSB.Builder
xBSB = "test テスト"

xSBS :: SBS.ShortByteString
xSBS = "test \xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88"

xiS :: String
xiS = "test \xfffd"

xiT :: T.Text
xiT = "test \xfffd"

xiTL :: TL.Text
xiTL = "test \xfffd"

xiTLB :: TLB.Builder
xiTLB = "test \xfffd"

xiST :: ST.ShortText
xiST = "test \xfffd"

xiBS :: BS.ByteString
xiBS = "test \xe3"

xiBSL :: BSL.ByteString
xiBSL = "test \xe3"

xiBSB :: BSB.Builder
xiBSB = "test \xfffd"

xiSBS :: SBS.ShortByteString
xiSBS = "test \xfffd"

------------------------------------------------------------------------------

newtype PosInt = PosInt Int
  deriving (Eq, Show)

instance TTC.Parse PosInt where
  parse = TTC.asS $ \ s -> case readMaybe s of
    Just i
      | i >= 0 -> Right $ PosInt i
      | otherwise -> Left $ TTC.fromS "not positive"
    Nothing -> Left $ TTC.fromS "not an integer"

instance TTC.Render PosInt where
  render (PosInt i) = TTC.convert $ show i

answer :: PosInt
answer = PosInt 42

answerS :: String
answerS = "42"

answerT :: T.Text
answerT = "42"

answerTL :: TL.Text
answerTL = "42"

answerTLB :: TLB.Builder
answerTLB = "42"

answerST :: ST.ShortText
answerST = "42"

answerBS :: BS.ByteString
answerBS = "42"

answerBSL :: BSL.ByteString
answerBSL = "42"

answerBSB :: BSB.Builder
answerBSB = "42"

answerSBS :: SBS.ShortByteString
answerSBS = "42"

answerZ :: Int
answerZ = 42

data IntError = IntInvalid
  deriving (Eq, Show)

------------------------------------------------------------------------------

data Color
  = Red
  | Green
  | Blue
  | White
  | Black
  deriving (Bounded, Enum, Eq, Show)

instance Read Color where
  readsPrec _ = TTC.readsEnum True True

instance TTC.Render Color where
  render = TTC.fromT . \case
    Red   -> "red"
    Green -> "green"
    Blue  -> "blue"
    White -> "white"
    Black -> "black"

data ColorError
  = ColorInvalid
  | ColorAmbiguous
  deriving (Eq, Show)

redS :: String
redS = "red"

redT :: T.Text
redT = "red"

redTL :: TL.Text
redTL = "red"

redTLB :: TLB.Builder
redTLB = "red"

redST :: ST.ShortText
redST = "red"

redBS :: BS.ByteString
redBS = "red"

redBSL :: BSL.ByteString
redBSL = "red"

redBSB :: BSB.Builder
redBSB = "red"

redSBS :: SBS.ShortByteString
redSBS = "red"

------------------------------------------------------------------------------

newtype PartialParser = PartialParser String
  deriving (Eq, Show)

instance TTC.Parse PartialParser where
  parse = TTC.asS $ \case
    "" -> Left $ TTC.fromT undefined
    s  -> Right $ PartialParser s

------------------------------------------------------------------------------
-- $Textual

testConvert :: TestTree
testConvert = testGroup "convert"
    [ testCase "@@" $
        "test テスト" @=? TTC.convert @String @String "test テスト"
    , testCase "@_" $ xT @=? TTC.convert @String "test テスト"
    , testCase "_@" $ "test テスト" @=? TTC.convert @_ @TL.Text xT
    , testCase "S->S" $ xS @=? TTC.convert xS
    , testCase "S->T" $ xT @=? TTC.convert xS
    , testCase "S->TL" $ xTL @=? TTC.convert xS
    , testCase "S->TLB" $ xTLB @=? TTC.convert xS
    , testCase "S->ST" $ xST @=? TTC.convert xS
    , testCase "S->BS" $ xBS @=? TTC.convert xS
    , testCase "S->BSL" $ xBSL @=? TTC.convert xS
    , testCase "S->BSB" $ xBSB @=? TTC.convert xS
    , testCase "S->SBS" $ xSBS @=? TTC.convert xS
    , testCase "T->S" $ xS @=? TTC.convert xT
    , testCase "T->T" $ xT @=? TTC.convert xT
    , testCase "T->TL" $ xTL @=? TTC.convert xT
    , testCase "T->TLB" $ xTLB @=? TTC.convert xT
    , testCase "T->ST" $ xST @=? TTC.convert xT
    , testCase "T->BS" $ xBS @=? TTC.convert xT
    , testCase "T->BSL" $ xBSL @=? TTC.convert xT
    , testCase "T->BSB" $ xBSB @=? TTC.convert xT
    , testCase "T->SBS" $ xSBS @=? TTC.convert xT
    , testCase "TL->S" $ xS @=? TTC.convert xTL
    , testCase "TL->T" $ xT @=? TTC.convert xTL
    , testCase "TL->TL" $ xTL @=? TTC.convert xTL
    , testCase "TL->TLB" $ xTLB @=? TTC.convert xTL
    , testCase "TL->ST" $ xST @=? TTC.convert xTL
    , testCase "TL->BS" $ xBS @=? TTC.convert xTL
    , testCase "TL->BSL" $ xBSL @=? TTC.convert xTL
    , testCase "TL->BSB" $ xBSB @=? TTC.convert xTL
    , testCase "TL->SBS" $ xSBS @=? TTC.convert xTL
    , testCase "TLB->S" $ xS @=? TTC.convert xTLB
    , testCase "TLB->T" $ xT @=? TTC.convert xTLB
    , testCase "TLB->TL" $ xTL @=? TTC.convert xTLB
    , testCase "TLB->TLB" $ xTLB @=? TTC.convert xTLB
    , testCase "TLB->ST" $ xST @=? TTC.convert xTLB
    , testCase "TLB->BS" $ xBS @=? TTC.convert xTLB
    , testCase "TLB->BSL" $ xBSL @=? TTC.convert xTLB
    , testCase "TLB->BSB" $ xBSB @=? TTC.convert xTLB
    , testCase "TLB->SBS" $ xSBS @=? TTC.convert xTLB
    , testCase "BS->S" $ xS @=? TTC.convert xBS
    , testCase "BS->T" $ xT @=? TTC.convert xBS
    , testCase "BS->TL" $ xTL @=? TTC.convert xBS
    , testCase "BS->TLB" $ xTLB @=? TTC.convert xBS
    , testCase "BS->ST" $ xST @=? TTC.convert xBS
    , testCase "BS->BS" $ xBS @=? TTC.convert xBS
    , testCase "BS->BSL" $ xBSL @=? TTC.convert xBS
    , testCase "BS->BSB" $ xBSB @=? TTC.convert xBS
    , testCase "BS->SBS" $ xSBS @=? TTC.convert xBS
    , testCase "BSL->S" $ xS @=? TTC.convert xBSL
    , testCase "BSL->T" $ xT @=? TTC.convert xBSL
    , testCase "BSL->TL" $ xTL @=? TTC.convert xBSL
    , testCase "BSL->TLB" $ xTLB @=? TTC.convert xBSL
    , testCase "BSL->ST" $ xST @=? TTC.convert xBSL
    , testCase "BSL->BS" $ xBS @=? TTC.convert xBSL
    , testCase "BSL->BSL" $ xBSL @=? TTC.convert xBSL
    , testCase "BSL->BSB" $ xBSB @=? TTC.convert xBSL
    , testCase "BSL->SBS" $ xSBS @=? TTC.convert xBSL
    , testCase "BSB->S" $ xS @=? TTC.convert xBSB
    , testCase "BSB->T" $ xT @=? TTC.convert xBSB
    , testCase "BSB->TL" $ xTL @=? TTC.convert xBSB
    , testCase "BSB->TLB" $ xTLB @=? TTC.convert xBSB
    , testCase "BSB->ST" $ xST @=? TTC.convert xBSB
    , testCase "BSB->BS" $ xBS @=? TTC.convert xBSB
    , testCase "BSB->BSL" $ xBSL @=? TTC.convert xBSB
    , testCase "BSB->BSB" $ xBSB @=? TTC.convert xBSB
    , testCase "BSB->SBS" $ xSBS @=? TTC.convert xBSB
    , testCase "SBS->S" $ xS @=? TTC.convert xSBS
    , testCase "SBS->T" $ xT @=? TTC.convert xSBS
    , testCase "SBS->TL" $ xTL @=? TTC.convert xSBS
    , testCase "SBS->TLB" $ xTLB @=? TTC.convert xSBS
    , testCase "SBS->ST" $ xST @=? TTC.convert xSBS
    , testCase "SBS->BS" $ xBS @=? TTC.convert xSBS
    , testCase "SBS->BSL" $ xBSL @=? TTC.convert xSBS
    , testCase "SBS->BSB" $ xBSB @=? TTC.convert xSBS
    , testCase "SBS->SBS" $ xSBS @=? TTC.convert xSBS
    ]

------------------------------------------------------------------------------

testToS :: TestTree
testToS = testGroup "toS"
    [ testCase "@" $ xS @=? TTC.toS @T.Text "test テスト"
    , testCase "S" $ xS @=? TTC.toS xS
    , testCase "T" $ xS @=? TTC.toS xT
    , testCase "TL" $ xS @=? TTC.toS xTL
    , testCase "TLB" $ xS @=? TTC.toS xTLB
    , testCase "ST" $ xS @=? TTC.toS xST
    , testCase "BS" $ xS @=? TTC.toS xBS
    , testCase "BS/invalid" $ xiS @=? TTC.toS xiBS
    , testCase "BSL" $ xS @=? TTC.toS xBSL
    , testCase "BSL/invalid" $ xiS @=? TTC.toS xiBSL
    , testCase "BSB" $ xS @=? TTC.toS xBSB
    , testCase "BSB/invalid" $ xiS @=? TTC.toS xiBSB
    , testCase "SBS" $ xS @=? TTC.toS xSBS
    , testCase "SBS/invalid" $ xiS @=? TTC.toS xiSBS
    ]

testToT :: TestTree
testToT = testGroup "toT"
    [ testCase "@" $ xT @=? TTC.toT @TL.Text "test テスト"
    , testCase "S" $ xT @=? TTC.toT xS
    , testCase "T" $ xT @=? TTC.toT xT
    , testCase "TL" $ xT @=? TTC.toT xTL
    , testCase "TLB" $ xT @=? TTC.toT xTLB
    , testCase "ST" $ xT @=? TTC.toT xST
    , testCase "BS" $ xT @=? TTC.toT xBS
    , testCase "BS/invalid" $ xiT @=? TTC.toT xiBS
    , testCase "BSL" $ xT @=? TTC.toT xBSL
    , testCase "BSL/invalid" $ xiT @=? TTC.toT xiBSL
    , testCase "BSB" $  xT @=? TTC.toT xBSB
    , testCase "BSB/invalid" $ xiT @=? TTC.toT xiBSB
    , testCase "SBS" $  xT @=? TTC.toT xSBS
    , testCase "SBS/invalid" $ xiT @=? TTC.toT xiSBS
    ]

testToTL :: TestTree
testToTL = testGroup "toTL"
    [ testCase "@" $ xTL @=? TTC.toTL @T.Text "test テスト"
    , testCase "S" $ xTL @=? TTC.toTL xS
    , testCase "T" $ xTL @=? TTC.toTL xT
    , testCase "TL" $ xTL @=? TTC.toTL xTL
    , testCase "TLB" $ xTL @=? TTC.toTL xTLB
    , testCase "ST" $ xTL @=? TTC.toTL xST
    , testCase "BS" $ xTL @=? TTC.toTL xBS
    , testCase "BS/invalid" $ xiTL @=? TTC.toTL xiBS
    , testCase "BSL" $ xTL @=? TTC.toTL xBSL
    , testCase "BSL/invalid" $ xiTL @=? TTC.toTL xiBSL
    , testCase "BSB" $ xTL @=? TTC.toTL xBSB
    , testCase "BSB/invalid" $ xiTL @=? TTC.toTL xiBSB
    , testCase "SBS" $ xTL @=? TTC.toTL xSBS
    , testCase "SBS/invalid" $ xiTL @=? TTC.toTL xiSBS
    ]

testToTLB :: TestTree
testToTLB = testGroup "toTLB"
    [ testCase "@" $ xTLB @=? TTC.toTLB @TL.Text "test テスト"
    , testCase "S" $ xTLB @=? TTC.toTLB xS
    , testCase "T" $ xTLB @=? TTC.toTLB xT
    , testCase "TL" $ xTLB @=? TTC.toTLB xTL
    , testCase "TLB" $ xTLB @=? TTC.toTLB xTLB
    , testCase "ST" $ xTLB @=? TTC.toTLB xST
    , testCase "BS" $ xTLB @=? TTC.toTLB xBS
    , testCase "BS/invalid" $ xiTLB @=? TTC.toTLB xiBS
    , testCase "BSL" $ xTLB @=? TTC.toTLB xBSL
    , testCase "BSL/invalid" $ xiTLB @=? TTC.toTLB xiBSL
    , testCase "BSB" $ xTLB @=? TTC.toTLB xBSB
    , testCase "BSB/invalid" $ xiTLB @=? TTC.toTLB xiBSB
    , testCase "SBS" $ xTLB @=? TTC.toTLB xSBS
    , testCase "SBS/invalid" $ xiTLB @=? TTC.toTLB xiSBS
    ]

testToST :: TestTree
testToST = testGroup "toST"
    [ testCase "@" $ xST @=? TTC.toST @T.Text "test テスト"
    , testCase "S" $ xST @=? TTC.toST xS
    , testCase "T" $ xST @=? TTC.toST xT
    , testCase "TL" $ xST @=? TTC.toST xTL
    , testCase "TLB" $ xST @=? TTC.toST xTLB
    , testCase "ST" $ xST @=? TTC.toST xST
    , testCase "BS" $ xST @=? TTC.toST xBS
    , testCase "BS/invalid" $ xiST @=? TTC.toST xiBS
    , testCase "BSL" $ xST @=? TTC.toST xBSL
    , testCase "BSL/invalid" $ xiST @=? TTC.toST xiBSL
    , testCase "BSB" $ xST @=? TTC.toST xBSB
    , testCase "BSB/invalid" $ xiST @=? TTC.toST xiBSB
    , testCase "SBS" $ xST @=? TTC.toST xSBS
    , testCase "SBS/invalid" $ xiST @=? TTC.toST xiSBS
    ]

testToBS :: TestTree
testToBS = testGroup "toBS"
    [ testCase "@" $ xBS @=? TTC.toBS @T.Text "test テスト"
    , testCase "S" $ xBS @=? TTC.toBS xS
    , testCase "T" $ xBS @=? TTC.toBS xT
    , testCase "TL" $ xBS @=? TTC.toBS xTL
    , testCase "TLB" $ xBS @=? TTC.toBS xTLB
    , testCase "ST" $ xBS @=? TTC.toBS xST
    , testCase "BS" $ xBS @=? TTC.toBS xBS
    , testCase "BSL" $ xBS @=? TTC.toBS xBSL
    , testCase "BSB" $ xBS @=? TTC.toBS xBSB
    , testCase "SBS" $ xBS @=? TTC.toBS xSBS
    ]

testToBSL :: TestTree
testToBSL = testGroup "toBSL"
    [ testCase "@" $ xBSL @=? TTC.toBSL @TL.Text "test テスト"
    , testCase "S" $ xBSL @=? TTC.toBSL xS
    , testCase "T" $ xBSL @=? TTC.toBSL xT
    , testCase "TL" $ xBSL @=? TTC.toBSL xTL
    , testCase "TLB" $ xBSL @=? TTC.toBSL xTLB
    , testCase "ST" $ xBSL @=? TTC.toBSL xST
    , testCase "BS" $ xBSL @=? TTC.toBSL xBS
    , testCase "BSL" $ xBSL @=? TTC.toBSL xBSL
    , testCase "BSB" $ xBSL @=? TTC.toBSL xBSB
    , testCase "SBS" $ xBSL @=? TTC.toBSL xSBS
    ]

testToBSB :: TestTree
testToBSB = testGroup "toBSB"
    [ testCase "@" $ xBSB @=? TTC.toBSB @TL.Text "test テスト"
    , testCase "S" $ xBSB @=? TTC.toBSB xS
    , testCase "T" $ xBSB @=? TTC.toBSB xT
    , testCase "TL" $ xBSB @=? TTC.toBSB xTL
    , testCase "TLB" $ xBSB @=? TTC.toBSB xTLB
    , testCase "ST" $ xBSB @=? TTC.toBSB xST
    , testCase "BS" $ xBSB @=? TTC.toBSB xBS
    , testCase "BSL" $ xBSB @=? TTC.toBSB xBSL
    , testCase "BSB" $ xBSB @=? TTC.toBSB xBSB
    , testCase "SBS" $ xBSB @=? TTC.toBSB xSBS
    ]

testToSBS :: TestTree
testToSBS = testGroup "toSBS"
    [ testCase "@" $ xSBS @=? TTC.toSBS @T.Text "test テスト"
    , testCase "S" $ xSBS @=? TTC.toSBS xS
    , testCase "T" $ xSBS @=? TTC.toSBS xT
    , testCase "TL" $ xSBS @=? TTC.toSBS xTL
    , testCase "TLB" $ xSBS @=? TTC.toSBS xTLB
    , testCase "ST" $ xSBS @=? TTC.toSBS xST
    , testCase "BS" $ xSBS @=? TTC.toSBS xBS
    , testCase "BSL" $ xSBS @=? TTC.toSBS xBSL
    , testCase "BSB" $ xSBS @=? TTC.toSBS xBSB
    , testCase "SBS" $ xSBS @=? TTC.toSBS xSBS
    ]

------------------------------------------------------------------------------

testFromS :: TestTree
testFromS = testGroup "fromS"
    [ testCase "@" $ "test テスト" @=? TTC.fromS @String xS
    , testCase "S" $ xS @=? TTC.fromS xS
    , testCase "T" $ xT @=? TTC.fromS xS
    , testCase "TL" $ xTL @=? TTC.fromS xS
    , testCase "TLB" $ xTLB @=? TTC.fromS xS
    , testCase "ST" $ xST @=? TTC.fromS xS
    , testCase "BS" $ xBS @=? TTC.fromS xS
    , testCase "BSL" $ xBSL @=? TTC.fromS xS
    , testCase "BSB" $ xBSB @=? TTC.fromS xS
    , testCase "SBS" $ xSBS @=? TTC.fromS xS
    ]

testFromT :: TestTree
testFromT = testGroup "fromT"
    [ testCase "@" $ "test テスト" @=? TTC.fromT @String xT
    , testCase "S" $ xS @=? TTC.fromT xT
    , testCase "T" $ xT @=? TTC.fromT xT
    , testCase "TL" $ xTL @=? TTC.fromT xT
    , testCase "TLB" $ xTLB @=? TTC.fromT xT
    , testCase "ST" $ xST @=? TTC.fromT xT
    , testCase "BS" $ xBS @=? TTC.fromT xT
    , testCase "BSL" $ xBSL @=? TTC.fromT xT
    , testCase "BSB" $ xBSB @=? TTC.fromT xT
    , testCase "SBS" $ xSBS @=? TTC.fromT xT
    ]

testFromTL :: TestTree
testFromTL = testGroup "fromTL"
    [ testCase "@" $ "test テスト" @=? TTC.fromTL @String xTL
    , testCase "S" $ xS @=? TTC.fromTL xTL
    , testCase "T" $ xT @=? TTC.fromTL xTL
    , testCase "TL" $ xTL @=? TTC.fromTL xTL
    , testCase "TLB" $ xTLB @=? TTC.fromTL xTL
    , testCase "ST" $ xST @=? TTC.fromTL xTL
    , testCase "BS" $ xBS @=? TTC.fromTL xTL
    , testCase "BSL" $ xBSL @=? TTC.fromTL xTL
    , testCase "BSB" $ xBSB @=? TTC.fromTL xTL
    , testCase "SBS" $ xSBS @=? TTC.fromTL xTL
    ]

testFromTLB :: TestTree
testFromTLB = testGroup "fromTLB"
    [ testCase "@" $ "test テスト" @=? TTC.fromTLB @String xTLB
    , testCase "S" $ xS @=? TTC.fromTLB xTLB
    , testCase "T" $ xT @=? TTC.fromTLB xTLB
    , testCase "TL" $ xTL @=? TTC.fromTLB xTLB
    , testCase "TLB" $ xTLB @=? TTC.fromTLB xTLB
    , testCase "ST" $ xST @=? TTC.fromTLB xTLB
    , testCase "BS" $ xBS @=? TTC.fromTLB xTLB
    , testCase "BSL" $ xBSL @=? TTC.fromTLB xTLB
    , testCase "BSB" $ xBSB @=? TTC.fromTLB xTLB
    , testCase "SBS" $ xSBS @=? TTC.fromTLB xTLB
    ]

testFromBS :: TestTree
testFromBS = testGroup "fromBS"
    [ testCase "@" $ "test テスト" @=? TTC.fromBS @String xBS
    , testCase "S" $ xS @=? TTC.fromBS xBS
    , testCase "T" $ xT @=? TTC.fromBS xBS
    , testCase "TL" $ xTL @=? TTC.fromBS xBS
    , testCase "TLB" $ xTLB @=? TTC.fromBS xBS
    , testCase "ST" $ xST @=? TTC.fromBS xBS
    , testCase "BS" $ xBS @=? TTC.fromBS xBS
    , testCase "BSL" $ xBSL @=? TTC.fromBS xBS
    , testCase "BSB" $ xBSB @=? TTC.fromBS xBS
    , testCase "SBS" $ xSBS @=? TTC.fromBS xBS
    ]

testFromBSL :: TestTree
testFromBSL = testGroup "fromBSL"
    [ testCase "@" $ "test テスト" @=? TTC.fromBSL @String xBSL
    , testCase "S" $ xS @=? TTC.fromBSL xBSL
    , testCase "T" $ xT @=? TTC.fromBSL xBSL
    , testCase "TL" $ xTL @=? TTC.fromBSL xBSL
    , testCase "TLB" $ xTLB @=? TTC.fromBSL xBSL
    , testCase "ST" $ xST @=? TTC.fromBSL xBSL
    , testCase "BS" $ xBS @=? TTC.fromBSL xBSL
    , testCase "BSL" $ xBSL @=? TTC.fromBSL xBSL
    , testCase "BSB" $ xBSB @=? TTC.fromBSL xBSL
    , testCase "SBS" $ xSBS @=? TTC.fromBSL xBSL
    ]

testFromBSB :: TestTree
testFromBSB = testGroup "fromBSB"
    [ testCase "@" $ "test テスト" @=? TTC.fromBSB @String xBSB
    , testCase "S" $ xS @=? TTC.fromBSB xBSB
    , testCase "T" $ xT @=? TTC.fromBSB xBSB
    , testCase "TL" $ xTL @=? TTC.fromBSB xBSB
    , testCase "TLB" $ xTLB @=? TTC.fromBSB xBSB
    , testCase "ST" $ xST @=? TTC.fromBSB xBSB
    , testCase "BS" $ xBS @=? TTC.fromBSB xBSB
    , testCase "BSL" $ xBSL @=? TTC.fromBSB xBSB
    , testCase "BSB" $ xBSB @=? TTC.fromBSB xBSB
    , testCase "SBS" $ xSBS @=? TTC.fromBSB xBSB
    ]

testFromSBS :: TestTree
testFromSBS = testGroup "fromSBS"
    [ testCase "@" $ "test テスト" @=? TTC.fromSBS @String xSBS
    , testCase "S" $ xS @=? TTC.fromSBS xSBS
    , testCase "T" $ xT @=? TTC.fromSBS xSBS
    , testCase "TL" $ xTL @=? TTC.fromSBS xSBS
    , testCase "TLB" $ xTLB @=? TTC.fromSBS xSBS
    , testCase "ST" $ xST @=? TTC.fromSBS xSBS
    , testCase "BS" $ xBS @=? TTC.fromSBS xSBS
    , testCase "BSL" $ xBSL @=? TTC.fromSBS xSBS
    , testCase "BSB" $ xBSB @=? TTC.fromSBS xSBS
    , testCase "SBS" $ xSBS @=? TTC.fromSBS xSBS
    ]

------------------------------------------------------------------------------

testAsS :: TestTree
testAsS = testGroup "asS"
    [ testCase "@" $ xS @=? TTC.asS @T.Text id "test テスト"
    , testCase "S" $ xS @=? TTC.asS id xS
    , testCase "T" $ xS @=? TTC.asS id xT
    , testCase "TL" $ xS @=? TTC.asS id xTL
    , testCase "TLB" $ xS @=? TTC.asS id xTLB
    , testCase "ST" $ xS @=? TTC.asS id xST
    , testCase "BS" $ xS @=? TTC.asS id xBS
    , testCase "BSL" $ xS @=? TTC.asS id xBSL
    , testCase "BSB" $ xS @=? TTC.asS id xBSB
    , testCase "SBS" $ xS @=? TTC.asS id xSBS
    ]

testAsT :: TestTree
testAsT = testGroup "asT"
    [ testCase "@" $ xT @=? TTC.asT @String id "test テスト"
    , testCase "S" $ xT @=? TTC.asT id xS
    , testCase "T" $ xT @=? TTC.asT id xT
    , testCase "TL" $ xT @=? TTC.asT id xTL
    , testCase "TLB" $ xT @=? TTC.asT id xTLB
    , testCase "ST" $ xT @=? TTC.asT id xST
    , testCase "BS" $ xT @=? TTC.asT id xBS
    , testCase "BSL" $ xT @=? TTC.asT id xBSL
    , testCase "BSB" $ xT @=? TTC.asT id xBSB
    , testCase "SBS" $ xT @=? TTC.asT id xSBS
    ]

testAsTL :: TestTree
testAsTL = testGroup "asTL"
    [ testCase "@" $ xTL @=? TTC.asTL @T.Text id "test テスト"
    , testCase "S" $ xTL @=? TTC.asTL id xS
    , testCase "T" $ xTL @=? TTC.asTL id xT
    , testCase "TL" $ xTL @=? TTC.asTL id xTL
    , testCase "TLB" $ xTL @=? TTC.asTL id xTLB
    , testCase "ST" $ xTL @=? TTC.asTL id xST
    , testCase "BS" $ xTL @=? TTC.asTL id xBS
    , testCase "BSL" $ xTL @=? TTC.asTL id xBSL
    , testCase "BSB" $ xTL @=? TTC.asTL id xBSB
    , testCase "SBS" $ xTL @=? TTC.asTL id xSBS
    ]

testAsTLB :: TestTree
testAsTLB = testGroup "asTLB"
    [ testCase "@" $ xTLB @=? TTC.asTLB @TL.Text id "test テスト"
    , testCase "S" $ xTLB @=? TTC.asTLB id xS
    , testCase "T" $ xTLB @=? TTC.asTLB id xT
    , testCase "TL" $ xTLB @=? TTC.asTLB id xTL
    , testCase "TLB" $ xTLB @=? TTC.asTLB id xTLB
    , testCase "ST" $ xTLB @=? TTC.asTLB id xST
    , testCase "BS" $ xTLB @=? TTC.asTLB id xBS
    , testCase "BSL" $ xTLB @=? TTC.asTLB id xBSL
    , testCase "BSB" $ xTLB @=? TTC.asTLB id xBSB
    , testCase "SBS" $ xTLB @=? TTC.asTLB id xSBS
    ]

testAsBS :: TestTree
testAsBS = testGroup "asBS"
    [ testCase "@" $ xBS @=? TTC.asBS @T.Text id "test テスト"
    , testCase "S" $ xBS @=? TTC.asBS id xS
    , testCase "T" $ xBS @=? TTC.asBS id xT
    , testCase "TL" $ xBS @=? TTC.asBS id xTL
    , testCase "TLB" $ xBS @=? TTC.asBS id xTLB
    , testCase "ST" $ xBS @=? TTC.asBS id xST
    , testCase "BS" $ xBS @=? TTC.asBS id xBS
    , testCase "BSL" $ xBS @=? TTC.asBS id xBSL
    , testCase "BSB" $ xBS @=? TTC.asBS id xBSB
    , testCase "SBS" $ xBS @=? TTC.asBS id xSBS
    ]

testAsBSL :: TestTree
testAsBSL = testGroup "asBSL"
    [ testCase "@" $ xBSL @=? TTC.asBSL @String id "test テスト"
    , testCase "S" $ xBSL @=? TTC.asBSL id xS
    , testCase "T" $ xBSL @=? TTC.asBSL id xT
    , testCase "TL" $ xBSL @=? TTC.asBSL id xTL
    , testCase "TLB" $ xBSL @=? TTC.asBSL id xTLB
    , testCase "ST" $ xBSL @=? TTC.asBSL id xST
    , testCase "BS" $ xBSL @=? TTC.asBSL id xBS
    , testCase "BSL" $ xBSL @=? TTC.asBSL id xBSL
    , testCase "BSB" $ xBSL @=? TTC.asBSL id xBSB
    , testCase "SBS" $ xBSL @=? TTC.asBSL id xSBS
    ]

testAsBSB :: TestTree
testAsBSB = testGroup "asBSB"
    [ testCase "@" $ xBSB @=? TTC.asBSB @String id "test テスト"
    , testCase "S" $ xBSB @=? TTC.asBSB id xS
    , testCase "T" $ xBSB @=? TTC.asBSB id xT
    , testCase "TL" $ xBSB @=? TTC.asBSB id xTL
    , testCase "TLB" $ xBSB @=? TTC.asBSB id xTLB
    , testCase "ST" $ xBSB @=? TTC.asBSB id xST
    , testCase "BS" $ xBSB @=? TTC.asBSB id xBS
    , testCase "BSL" $ xBSB @=? TTC.asBSB id xBSL
    , testCase "BSB" $ xBSB @=? TTC.asBSB id xBSB
    , testCase "SBS" $ xBSB @=? TTC.asBSB id xSBS
    ]

testAsSBS :: TestTree
testAsSBS = testGroup "asSBS"
    [ testCase "@" $ xSBS @=? TTC.asSBS @String id "test テスト"
    , testCase "S" $ xSBS @=? TTC.asSBS id xS
    , testCase "T" $ xSBS @=? TTC.asSBS id xT
    , testCase "TL" $ xSBS @=? TTC.asSBS id xTL
    , testCase "TLB" $ xSBS @=? TTC.asSBS id xTLB
    , testCase "ST" $ xSBS @=? TTC.asSBS id xST
    , testCase "BS" $ xSBS @=? TTC.asSBS id xBS
    , testCase "BSL" $ xSBS @=? TTC.asSBS id xBSL
    , testCase "BSB" $ xSBS @=? TTC.asSBS id xBSB
    , testCase "SBS" $ xSBS @=? TTC.asSBS id xSBS
    ]

------------------------------------------------------------------------------
-- $Render

testRender :: TestTree
testRender = testGroup "render"
    [ testCase "_@" $ "42" @=? TTC.render @_ @String answer
    , testCase "S" $ answerS @=? TTC.render answer
    , testCase "T" $ answerT @=? TTC.render answer
    , testCase "TL" $ answerTL @=? TTC.render answer
    , testCase "TLB" $ answerTLB @=? TTC.render answer
    , testCase "ST" $ answerST @=? TTC.render answer
    , testCase "BS" $ answerBS @=? TTC.render answer
    , testCase "BSL" $ answerBSL @=? TTC.render answer
    , testCase "BSB" $ answerBSB @=? TTC.render answer
    , testCase "SBS" $ answerSBS @=? TTC.render answer
    ]

------------------------------------------------------------------------------

testRenderDefault :: TestTree
testRenderDefault = testGroup "RenderDefault"
    [ testCase "Bool" $ "True" @=? TTC.render @_ @String True
    , testCase "Char" $ "*" @=? TTC.renderS '*'
    , let x = 3.14159 :: Double
      in  testCase "Double" $ show x @=? TTC.renderS x
    , let x = 3.14159 :: Float
      in  testCase "Float" $ show x @=? TTC.renderS x
    , let n = 42 :: Integer
      in  testCase "Integer" $ show n @=? TTC.renderS n
    , let n = 42 :: Int
      in  testCase "Int" $ show n @=? TTC.renderS n
    , let n = 42 :: Int8
      in  testCase "Int8" $ show n @=? TTC.renderS n
    , let n = 42 :: Int16
      in  testCase "Int16" $ show n @=? TTC.renderS n
    , let n = 42 :: Int32
      in  testCase "Int32" $ show n @=? TTC.renderS n
    , let n = 42 :: Int64
      in  testCase "Int64" $ show n @=? TTC.renderS n
    , let w = 42 :: Word
      in  testCase "Word" $ show w @=? TTC.renderS w
    , let w = 42 :: Word8
      in  testCase "Word8" $ show w @=? TTC.renderS w
    , let w = 42 :: Word16
      in  testCase "Word16" $ show w @=? TTC.renderS w
    , let w = 42 :: Word32
      in  testCase "Word32" $ show w @=? TTC.renderS w
    , let w = 42 :: Word64
      in  testCase "Word64" $ show w @=? TTC.renderS w
    , testCase "String" $ xS @=? TTC.renderS xS
    , testCase "T.Text" $ xS @=? TTC.renderS xT
    , testCase "TL.Text" $ xS @=? TTC.renderS xTL
    , testCase "TLB.Builder" $ xS @=? TTC.renderS xTLB
    , testCase "ST.ShortText" $ xS @=? TTC.renderS xST
    , testCase "BS.ByteString" $ xS @=? TTC.renderS xBS
    , testCase "BSL.ByteString" $ xS @=? TTC.renderS xBSL
    , testCase "BSB.Builder" $ xS @=? TTC.renderS xBSB
    , testCase "SBS.ShortByteString" $ xS @=? TTC.renderS xSBS
    ]

------------------------------------------------------------------------------

testRenderWithShow :: TestTree
testRenderWithShow = testGroup "renderWithShow"
    [ testCase "@" $ "42" @=? TTC.renderWithShow @String answerZ
    , testCase "S" $ answerS @=? TTC.renderWithShow answerZ
    , testCase "T" $ answerT @=? TTC.renderWithShow answerZ
    , testCase "TL" $ answerTL @=? TTC.renderWithShow answerZ
    , testCase "TLB" $ answerTLB @=? TTC.renderWithShow answerZ
    , testCase "ST" $ answerST @=? TTC.renderWithShow answerZ
    , testCase "BS" $ answerBS @=? TTC.renderWithShow answerZ
    , testCase "BSL" $ answerBSL @=? TTC.renderWithShow answerZ
    , testCase "BSB" $ answerBSB @=? TTC.renderWithShow answerZ
    , testCase "SBS" $ answerSBS @=? TTC.renderWithShow answerZ
    ]

------------------------------------------------------------------------------

testRenderS :: TestTree
testRenderS = testCase "renderS" $ answerS @=? TTC.renderS answer

testRenderT :: TestTree
testRenderT = testCase "renderT" $ answerT @=? TTC.renderT answer

testRenderTL :: TestTree
testRenderTL = testCase "renderTL" $ answerTL @=? TTC.renderTL answer

testRenderTLB :: TestTree
testRenderTLB = testCase "renderTLB" $ answerTLB @=? TTC.renderTLB answer

testRenderST :: TestTree
testRenderST = testCase "renderST" $ answerST @=? TTC.renderST answer

testRenderBS :: TestTree
testRenderBS = testCase "renderBS" $ answerBS @=? TTC.renderBS answer

testRenderBSL :: TestTree
testRenderBSL = testCase "renderBSL" $ answerBSL @=? TTC.renderBSL answer

testRenderBSB :: TestTree
testRenderBSB = testCase "renderBSB" $
    answerBSL @=? BSB.toLazyByteString (TTC.renderBSB answer)

testRenderSBS :: TestTree
testRenderSBS = testCase "renderSBS" $ answerSBS @=? TTC.renderSBS answer

------------------------------------------------------------------------------
-- $Parse

testParse :: TestTree
testParse = testGroup "parse"
    [ testCase "_@@" $ Right answer @=? TTC.parse @_ @String @String "42"
    , testCase "S" $ Right answer @=? parse answerS
    , testCase "T" $ Right answer @=? parse answerT
    , testCase "TL" $ Right answer @=? parse answerTL
    , testCase "TLB" $ Right answer @=? parse answerTLB
    , testCase "ST" $ Right answer @=? parse answerST
    , testCase "BS" $ Right answer @=? parse answerBS
    , testCase "BSL" $ Right answer @=? parse answerBSL
    , testCase "BSB" $ Right answer @=? parse answerBSB
    , testCase "SBS" $ Right answer @=? parse answerSBS
    , testCase "negative" $ Left "not positive" @=? parse @String "-42"
    , testCase "invalid" $ Left "not an integer" @=? parse @String "4a2"
    ]
  where
    parse :: TTC.Textual t => t -> Either String PosInt
    parse = TTC.parse

------------------------------------------------------------------------------


testParseDefault :: TestTree
testParseDefault = testGroup "ParseDefault"
    [ testGroup "Bool"
        [ testCase "True" $ Right True @=? parse "True"
        , testCase "False" $ Right False @=? parse "False"
        , testCase "invalid" $ Left "invalid Bool" @=? parse @Bool "false"
        ]
    , testGroup "Char"
        [ testCase "valid" $ Right '*' @=? parse "*"
        , testCase "empty" $ Left "invalid Char" @=? parse @Char ""
        , testCase "multiple" $ Left "invalid Char" @=? parse @Char "**"
        ]
    , let s = show (3.14159 :: Double)
      in  testGroup "Double"
            [ testCase "valid" $ Right (read s) @=? parse @Double s
            , testCase "invalid" $
                Left "invalid Double" @=? parse @Double "invalid"
            ]
    , let s = show (3.14159 :: Float)
      in  testGroup "Float"
            [ testCase "valid" $ Right (read s) @=? parse @Float s
            , testCase "invalid" $
                Left "invalid Float" @=? parse @Float "invalid"
            ]
    , testGroup "Integer"
        [ testCase "valid" $ Right 42 @=? parse @Integer "42"
        , testCase "invalid" $
            Left "invalid Integer" @=? parse @Integer "invalid"
        ]
    , testGroup "Int"
        [ testCase "valid" $ Right 42 @=? parse @Int "42"
        , testCase "invalid" $ Left "invalid Int" @=? parse @Int "invalid"
        ]
    , testGroup "Int8"
        [ testCase "valid" $ Right 42 @=? parse @Int8 "42"
        , testCase "invalid" $ Left "invalid Int8" @=? parse @Int8 "invalid"
        ]
    , testGroup "Int16"
        [ testCase "valid" $ Right 42 @=? parse @Int16 "42"
        , testCase "invalid" $ Left "invalid Int16" @=? parse @Int16 "invalid"
        ]
    , testGroup "Int32"
        [ testCase "valid" $ Right 42 @=? parse @Int32 "42"
        , testCase "invalid" $ Left "invalid Int32" @=? parse @Int32 "invalid"
        ]
    , testGroup "Int64"
        [ testCase "valid" $ Right 42 @=? parse @Int64 "42"
        , testCase "invalid" $ Left "invalid Int64" @=? parse @Int64 "invalid"
        ]
    , testGroup "Word"
        [ testCase "valid" $ Right 42 @=? parse @Word "42"
        , testCase "invalid" $ Left "invalid Word" @=? parse @Word "invalid"
        ]
    , testGroup "Word8"
        [ testCase "valid" $ Right 42 @=? parse @Word8 "42"
        , testCase "invalid" $ Left "invalid Word8" @=? parse @Word8 "invalid"
        ]
    , testGroup "Word16"
        [ testCase "valid" $ Right 42 @=? parse @Word16 "42"
        , testCase "invalid" $
            Left "invalid Word16" @=? parse @Word16 "invalid"
        ]
    , testGroup "Word32"
        [ testCase "valid" $ Right 42 @=? parse @Word32 "42"
        , testCase "invalid" $
            Left "invalid Word32" @=? parse @Word32 "invalid"
        ]
    , testGroup "Word64"
        [ testCase "valid" $ Right 42 @=? parse @Word64 "42"
        , testCase "invalid" $
            Left "invalid Word64" @=? parse @Word64 "invalid"
        ]
    , testGroup "String"
        [ testCase "empty" $ Right "" @=? parse @String ""
        , testCase "nonempty" $ Right xS @=? parse xS
        ]
    , testGroup "T.Text"
        [ testCase "empty" $ Right T.empty @=? parse ""
        , testCase "nonempty" $ Right xT @=? parse xS
        ]
    , testGroup "TL.Text"
        [ testCase "empty" $ Right TL.empty @=? parse ""
        , testCase "nonempty" $ Right xTL @=? parse xS
        ]
    , testGroup "TLB.Builder"
        [ testCase "empty" $ Right (TLB.fromString "") @=? parse ""
        , testCase "nonempty" $ Right xTLB @=? parse xS
        ]
    , testGroup "ST.ShortString"
        [ testCase "empty" $ Right ST.empty @=? parse ""
        , testCase "nonempty" $ Right xST @=? parse xS
        ]
    , testGroup "BS.ByteString"
        [ testCase "empty" $ Right BS.empty @=? parse ""
        , testCase "nonempty" $ Right xBS @=? parse xS
        ]
    , testGroup "BSL.ByteString"
        [ testCase "empty" $ Right BSL.empty @=? parse ""
        , testCase "nonempty" $ Right xBSL @=? parse xS
        ]
    , testGroup "BSB.Builder"
        [ testCase "empty" $ Right (BSB.byteString BS.empty) @=? parse ""
        , testCase "nonempty" $ Right xBSB @=? parse xS
        ]
    , testGroup "SBS.ShortByteString"
        [ testCase "empty" $ Right SBS.empty @=? parse ""
        , testCase "nonempty" $ Right xSBS @=? parse xS
        ]
    ]
  where
    parse :: TTC.Parse a => String -> Either String a
    parse = TTC.parse

------------------------------------------------------------------------------

testWithError :: TestTree
testWithError = testGroup "withError"
    [ testCase "valid" $
        Right answer @=? TTC.withError @String @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withError @String @String @PosInt "err" Nothing
    ]

testWithErrorS :: TestTree
testWithErrorS = testGroup "withErrorS"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorS @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorS @String @PosInt "err" Nothing
    ]

testWithErrorT :: TestTree
testWithErrorT = testGroup "withErrorT"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorT @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorT @String @PosInt "err" Nothing
    ]

testWithErrorTL :: TestTree
testWithErrorTL = testGroup "withErrorTL"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorTL @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorTL @String @PosInt "err" Nothing
    ]

testWithErrorTLB :: TestTree
testWithErrorTLB = testGroup "withErrorTLB"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorTLB @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorTLB @String @PosInt "err" Nothing
    ]

testWithErrorST :: TestTree
testWithErrorST = testGroup "withErrorST"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorST @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorST @String @PosInt "err" Nothing
    ]

testWithErrorBS :: TestTree
testWithErrorBS = testGroup "withErrorBS"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorBS @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorBS @String @PosInt "err" Nothing
    ]

testWithErrorBSL :: TestTree
testWithErrorBSL = testGroup "withErrorBSL"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorBSL @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorBSL @String @PosInt "err" Nothing
    ]

testWithErrorBSB :: TestTree
testWithErrorBSB = testGroup "withErrorBSB"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorBSB @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorBSB @String @PosInt "err" Nothing
    ]

testWithErrorSBS :: TestTree
testWithErrorSBS = testGroup "withErrorSBS"
    [ testCase "valid" $
        Right answer @=? TTC.withErrorSBS @String undefined (Just answer)
    , testCase "invalid" $
        Left "err" @=? TTC.withErrorSBS @String @PosInt "err" Nothing
    ]

------------------------------------------------------------------------------

testPrefixError :: TestTree
testPrefixError = testGroup "prefixError"
    [ testCase "valid" $
        Right answer
          @=? TTC.prefixError @String @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixError @String @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorS :: TestTree
testPrefixErrorS = testGroup "prefixErrorS"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorS @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorS @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorT :: TestTree
testPrefixErrorT = testGroup "prefixErrorT"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorT @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorT @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorTL :: TestTree
testPrefixErrorTL = testGroup "prefixErrorTL"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorTL @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorTL @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorTLB :: TestTree
testPrefixErrorTLB = testGroup "prefixErrorTLB"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorTLB @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorTLB @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorST :: TestTree
testPrefixErrorST = testGroup "prefixErrorST"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorST @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorST @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorBS :: TestTree
testPrefixErrorBS = testGroup "prefixErrorBS"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorBS @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorBS @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorBSL :: TestTree
testPrefixErrorBSL = testGroup "prefixErrorBSL"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorBSL @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorBSL @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorBSB :: TestTree
testPrefixErrorBSB = testGroup "prefixErrorBSB"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorBSB @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorBSB @String @PosInt "oops: " (Left "err")
    ]

testPrefixErrorSBS :: TestTree
testPrefixErrorSBS = testGroup "prefixErrorSBS"
    [ testCase "valid" $
        Right answer @=? TTC.prefixErrorSBS @String "oops: " (Right answer)
    , testCase "invalid" $
        Left "oops: err"
          @=? TTC.prefixErrorSBS @String @PosInt "oops: " (Left "err")
    ]

------------------------------------------------------------------------------

testParseWithRead :: TestTree
testParseWithRead = testGroup "parseWithRead"
    [ testCase "@" $
        Right answerZ @=? TTC.parseWithRead @String IntInvalid "42"
    , testCase "S" $ Right answerZ @=? parseWithRead answerS
    , testCase "T" $ Right answerZ @=? parseWithRead answerT
    , testCase "TL" $ Right answerZ @=? parseWithRead answerTL
    , testCase "TLB" $ Right answerZ @=? parseWithRead answerTLB
    , testCase "ST" $ Right answerZ @=? parseWithRead answerST
    , testCase "BS" $ Right answerZ @=? parseWithRead answerBS
    , testCase "BSL" $ Right answerZ @=? parseWithRead answerBSL
    , testCase "BSB" $ Right answerZ @=? parseWithRead answerBSB
    , testCase "SBS" $ Right answerZ @=? parseWithRead answerSBS
    , testCase "invalid" $ Left IntInvalid @=? parseWithRead @String "4a2"
    ]
  where
    parseWithRead :: TTC.Textual t => t -> Either IntError Int
    parseWithRead = TTC.parseWithRead IntInvalid

testParseWithRead' :: TestTree
testParseWithRead' = testGroup "parseWithRead'"
    [ testCase "@" $
        Right answerZ @=? TTC.parseWithRead' @String @String "Int" "42"
    , testCase "S" $ Right answerZ @=? parseWithRead' answerS
    , testCase "T" $ Right answerZ @=? parseWithRead' answerT
    , testCase "TL" $ Right answerZ @=? parseWithRead' answerTL
    , testCase "TLB" $ Right answerZ @=? parseWithRead' answerTLB
    , testCase "ST" $ Right answerZ @=? parseWithRead' answerST
    , testCase "BS" $ Right answerZ @=? parseWithRead' answerBS
    , testCase "BSL" $ Right answerZ @=? parseWithRead' answerBSL
    , testCase "BSB" $ Right answerZ @=? parseWithRead' answerBSB
    , testCase "SBS" $ Right answerZ @=? parseWithRead' answerSBS
    , testCase "invalid" $ Left "invalid Int" @=? parseWithRead' @String "4a2"
    ]
  where
    parseWithRead' :: TTC.Textual t => t -> Either String Int
    parseWithRead' = TTC.parseWithRead' "Int"

testMaybeParseWithRead :: TestTree
testMaybeParseWithRead = testGroup "maybeParseWithRead"
    [ testCase "@" $ Just answerZ @=? TTC.maybeParseWithRead @String "42"
    , testCase "S" $ Just answerZ @=? TTC.maybeParseWithRead answerS
    , testCase "T" $ Just answerZ @=? TTC.maybeParseWithRead answerT
    , testCase "TL" $ Just answerZ @=? TTC.maybeParseWithRead answerTL
    , testCase "TLB" $ Just answerZ @=? TTC.maybeParseWithRead answerTLB
    , testCase "ST" $ Just answerZ @=? TTC.maybeParseWithRead answerST
    , testCase "BS" $ Just answerZ @=? TTC.maybeParseWithRead answerBS
    , testCase "BSL" $ Just answerZ @=? TTC.maybeParseWithRead answerBSL
    , testCase "BSB" $ Just answerZ @=? TTC.maybeParseWithRead answerBSB
    , testCase "SBS" $ Just answerZ @=? TTC.maybeParseWithRead answerSBS
    , testCase "invalid" $
        Nothing @=? TTC.maybeParseWithRead @String @Int "4a2"
    ]

------------------------------------------------------------------------------

testParseEnum :: TestTree
testParseEnum = testGroup "parseEnum"
    [ testCase "@" $
        Right Red
          @=? TTC.parseEnum @String
            False False ColorInvalid ColorAmbiguous "red"
    , testCase "S" $ Right Red @=? parseEnum False False redS
    , testCase "T" $ Right Red @=? parseEnum False False redT
    , testCase "TL" $ Right Red @=? parseEnum False False redTL
    , testCase "TLB" $ Right Red @=? parseEnum False False redTLB
    , testCase "ST" $ Right Red @=? parseEnum False False redST
    , testCase "BS" $ Right Red @=? parseEnum False False redBS
    , testCase "BSL" $ Right Red @=? parseEnum False False redBSL
    , testCase "BSB" $ Right Red @=? parseEnum False False redBSB
    , testCase "SBS" $ Right Red @=? parseEnum False False redSBS
    , testCase "CI" $ Right Red @=? parseEnum @String True False "Red"
    , testCase "!CI" $
        Left ColorInvalid @=? parseEnum @String False False "Red"
    , testCase "prefix" $ Right Red @=? parseEnum @String False True "r"
    , testCase "ambiguous" $
        Left ColorAmbiguous @=? parseEnum @String False True "bl"
    ]
  where
    parseEnum :: TTC.Textual t => Bool -> Bool -> t -> Either ColorError Color
    parseEnum allowCI allowPrefix =
      TTC.parseEnum allowCI allowPrefix ColorInvalid ColorAmbiguous

testParseEnum' :: TestTree
testParseEnum' = testGroup "parseEnum'"
    [ testCase "@" $
        Right Red
          @=? TTC.parseEnum' @String @String "Color" False False "red"
    , testCase "S" $ Right Red @=? parseEnum' False False redS
    , testCase "T" $ Right Red @=? parseEnum' False False redT
    , testCase "TL" $ Right Red @=? parseEnum' False False redTL
    , testCase "TLB" $ Right Red @=? parseEnum' False False redTLB
    , testCase "ST" $ Right Red @=? parseEnum' False False redST
    , testCase "BS" $ Right Red @=? parseEnum' False False redBS
    , testCase "BSL" $ Right Red @=? parseEnum' False False redBSL
    , testCase "BSB" $ Right Red @=? parseEnum' False False redBSB
    , testCase "SBS" $ Right Red @=? parseEnum' False False redSBS
    , testCase "CI" $ Right Red @=? parseEnum' @String True False "Red"
    , testCase "!CI" $
        Left "invalid Color" @=? parseEnum' @String False False "Red"
    , testCase "prefix" $ Right Red @=? parseEnum' @String False True "r"
    , testCase "ambiguous" $
        Left "ambiguous Color" @=? parseEnum' @String False True "bl"
    ]
  where
    parseEnum' :: TTC.Textual t => Bool -> Bool -> t -> Either String Color
    parseEnum' = TTC.parseEnum' "Color"

------------------------------------------------------------------------------

testParseS :: TestTree
testParseS = testGroup "parseS"
    [ testCase "valid" $ Right answer @=? TTC.parseS @String answerS
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseS @String @PosInt "4a2"
    ]

testParseT :: TestTree
testParseT = testGroup "parseT"
    [ testCase "valid" $ Right answer @=? TTC.parseT @String answerT
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseT @String @PosInt "4a2"
    ]

testParseTL :: TestTree
testParseTL = testGroup "parseTL"
    [ testCase "valid" $ Right answer @=? TTC.parseTL @String answerTL
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseTL @String @PosInt "4a2"
    ]

testParseTLB :: TestTree
testParseTLB = testGroup "parseTLB"
    [ testCase "valid" $ Right answer @=? TTC.parseTLB @String answerTLB
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseTLB @String @PosInt "4a2"
    ]

testParseST :: TestTree
testParseST = testGroup "parseST"
    [ testCase "valid" $ Right answer @=? TTC.parseST @String answerST
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseST @String @PosInt "4a2"
    ]

testParseBS :: TestTree
testParseBS = testGroup "parseBS"
    [ testCase "valid" $ Right answer @=? TTC.parseBS @String answerBS
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseBS @String @PosInt "4a2"
    ]

testParseBSL :: TestTree
testParseBSL = testGroup "parseBSL"
    [ testCase "valid" $ Right answer @=? TTC.parseBSL @String answerBSL
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseBSL @String @PosInt "4a2"
    ]

testParseBSB :: TestTree
testParseBSB = testGroup "parseBSB"
    [ testCase "valid" $ Right answer @=? TTC.parseBSB @String answerBSB
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseBSB @String @PosInt "4a2"
    ]

testParseSBS :: TestTree
testParseSBS = testGroup "parseSBS"
    [ testCase "valid" $ Right answer @=? TTC.parseSBS @String answerSBS
    , testCase "invalid" $
        Left "not an integer" @=? TTC.parseSBS @String @PosInt "4a2"
    ]

------------------------------------------------------------------------------

testParseMaybe :: TestTree
testParseMaybe = testGroup "parseMaybe"
    [ testCase "@" $ Just answer @=? TTC.parseMaybe @String "42"
    , testCase "S" $ Just answer @=? TTC.parseMaybe answerS
    , testCase "T" $ Just answer @=? TTC.parseMaybe answerT
    , testCase "TL" $ Just answer @=? TTC.parseMaybe answerTL
    , testCase "TLB" $ Just answer @=? TTC.parseMaybe answerTLB
    , testCase "ST" $ Just answer @=? TTC.parseMaybe answerST
    , testCase "BS" $ Just answer @=? TTC.parseMaybe answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseMaybe answerBSL
    , testCase "BSB" $ Just answer @=? TTC.parseMaybe answerBSB
    , testCase "SBS" $ Just answer @=? TTC.parseMaybe answerSBS
    , testCase "noerror" $
        Just (PartialParser "test") @=? TTC.parseMaybe @String "test"
    , testCase "negative" $ Nothing @=? TTC.parseMaybe @String @PosInt "-42"
    , testCase "invalid" $ Nothing @=? TTC.parseMaybe @String @PosInt "4a2"
    ]

testParseMaybeS :: TestTree
testParseMaybeS = testGroup "parseMaybeS"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeS answerS
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeS @PosInt "4a2"
    ]

testParseMaybeT :: TestTree
testParseMaybeT = testGroup "parseMaybeT"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeT answerT
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeT @PosInt "4a2"
    ]

testParseMaybeTL :: TestTree
testParseMaybeTL = testGroup "parseMaybeTL"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeTL answerTL
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeTL @PosInt "4a2"
    ]

testParseMaybeTLB :: TestTree
testParseMaybeTLB = testGroup "parseMaybeTLB"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeTLB answerTLB
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeTLB @PosInt "4a2"
    ]

testParseMaybeST :: TestTree
testParseMaybeST = testGroup "parseMaybeST"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeST answerST
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeST @PosInt "4a2"
    ]

testParseMaybeBS :: TestTree
testParseMaybeBS = testGroup "parseMaybeBS"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeBS answerBS
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeBS @PosInt "4a2"
    ]

testParseMaybeBSL :: TestTree
testParseMaybeBSL = testGroup "parseMaybeBSL"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeBSL answerBSL
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeBSL @PosInt "4a2"
    ]

testParseMaybeBSB :: TestTree
testParseMaybeBSB = testGroup "parseMaybeBSB"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeBSB answerBSB
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeBSB @PosInt "4a2"
    ]

testParseMaybeSBS :: TestTree
testParseMaybeSBS = testGroup "parseMaybeSBS"
    [ testCase "valid" $ Just answer @=? TTC.parseMaybeSBS answerSBS
    , testCase "invalid" $ Nothing @=? TTC.parseMaybeSBS @PosInt "4a2"
    ]

------------------------------------------------------------------------------

testParseOrFail :: TestTree
testParseOrFail = testGroup "parseOrFail"
    [ testCase "@" $ Just answer @=? TTC.parseOrFail @String "42"
    , testCase "S" $ Just answer @=? TTC.parseOrFail answerS
    , testCase "T" $ Just answer @=? TTC.parseOrFail answerT
    , testCase "TL" $ Just answer @=? TTC.parseOrFail answerTL
    , testCase "TLB" $ Just answer @=? TTC.parseOrFail answerTLB
    , testCase "ST" $ Just answer @=? TTC.parseOrFail answerST
    , testCase "BS" $ Just answer @=? TTC.parseOrFail answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseOrFail answerBSL
    , testCase "BSB" $ Just answer @=? TTC.parseOrFail answerBSB
    , testCase "SBS" $ Just answer @=? TTC.parseOrFail answerSBS
    , testCase "noerror" $
        Just (PartialParser "test") @=? TTC.parseOrFail @String "test"
    , testCase "negative" $ Nothing @=? TTC.parseOrFail @String @PosInt "-42"
    , testCase "invalid" $ Nothing @=? TTC.parseOrFail @String @PosInt "4a2"
    ]

testParseOrFailS :: TestTree
testParseOrFailS = testGroup "parseOrFailS"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailS answerS
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailS @PosInt "4a2"
    ]

testParseOrFailT :: TestTree
testParseOrFailT = testGroup "parseOrFailT"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailT answerT
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailT @PosInt "4a2"
    ]

testParseOrFailTL :: TestTree
testParseOrFailTL = testGroup "parseOrFailTL"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailTL answerTL
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailTL @PosInt "4a2"
    ]

testParseOrFailTLB :: TestTree
testParseOrFailTLB = testGroup "parseOrFailTLB"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailTLB answerTLB
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailTLB @PosInt "4a2"
    ]

testParseOrFailST :: TestTree
testParseOrFailST = testGroup "parseOrFailST"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailST answerST
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailST @PosInt "4a2"
    ]

testParseOrFailBS :: TestTree
testParseOrFailBS = testGroup "parseOrFailBS"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailBS answerBS
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailBS @PosInt "4a2"
    ]

testParseOrFailBSL :: TestTree
testParseOrFailBSL = testGroup "parseOrFailBSL"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailBSL answerBSL
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailBSL @PosInt "4a2"
    ]

testParseOrFailBSB :: TestTree
testParseOrFailBSB = testGroup "parseOrFailBSB"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailBSB answerBSB
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailBSB @PosInt "4a2"
    ]

testParseOrFailSBS :: TestTree
testParseOrFailSBS = testGroup "parseOrFailSBS"
    [ testCase "valid" $ Just answer @=? TTC.parseOrFailSBS answerSBS
    , testCase "invalid" $ Nothing @=? TTC.parseOrFailSBS @PosInt "4a2"
    ]

------------------------------------------------------------------------------

testParseUnsafe :: TestTree
testParseUnsafe = testGroup "parseUnsafe"
    [ testCase "@" $ answer @=? TTC.parseUnsafe @String "42"
    , testCase "S" $ answer @=? TTC.parseUnsafe answerS
    , testCase "T" $ answer @=? TTC.parseUnsafe answerT
    , testCase "TL" $ answer @=? TTC.parseUnsafe answerTL
    , testCase "TLB" $ answer @=? TTC.parseUnsafe answerTLB
    , testCase "ST" $ answer @=? TTC.parseUnsafe answerST
    , testCase "BS" $ answer @=? TTC.parseUnsafe answerBS
    , testCase "BSL" $ answer @=? TTC.parseUnsafe answerBSL
    , testCase "BSB" $ answer @=? TTC.parseUnsafe answerBSB
    , testCase "SBS" $ answer @=? TTC.parseUnsafe answerSBS
    , testCase "negative" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafe @String @PosInt "-42"
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafe @String @PosInt "4a2"
    ]

testParseUnsafeS :: TestTree
testParseUnsafeS = testGroup "parseUnsafeS"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeS answerS
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeS @PosInt "4a2"
    ]

testParseUnsafeT :: TestTree
testParseUnsafeT = testGroup "parseUnsafeT"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeT answerT
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeT @PosInt "4a2"
    ]

testParseUnsafeTL :: TestTree
testParseUnsafeTL = testGroup "parseUnsafeTL"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeTL answerTL
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeTL @PosInt "4a2"
    ]

testParseUnsafeTLB :: TestTree
testParseUnsafeTLB = testGroup "parseUnsafeTLB"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeTLB answerTLB
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeTLB @PosInt "4a2"
    ]

testParseUnsafeST :: TestTree
testParseUnsafeST = testGroup "parseUnsafeST"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeST answerST
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeST @PosInt "4a2"
    ]

testParseUnsafeBS :: TestTree
testParseUnsafeBS = testGroup "parseUnsafeBS"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeBS answerBS
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeBS @PosInt "4a2"
    ]

testParseUnsafeBSL :: TestTree
testParseUnsafeBSL = testGroup "parseUnsafeBSL"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeBSL answerBSL
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeBSL @PosInt "4a2"
    ]

testParseUnsafeBSB :: TestTree
testParseUnsafeBSB = testGroup "parseUnsafeBSB"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeBSB answerBSB
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeBSB @PosInt "4a2"
    ]

testParseUnsafeSBS :: TestTree
testParseUnsafeSBS = testGroup "parseUnsafeSBS"
    [ testCase "valid" $ answer @=? TTC.parseUnsafeSBS answerSBS
    , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $
        TTC.parseUnsafeSBS @PosInt "4a2"
    ]

------------------------------------------------------------------------------

testReadsWithParse :: TestTree
testReadsWithParse = testGroup "readsWithParse"
    [ testCase "valid" $ [(answer, "")] @=? TTC.readsWithParse answerS
    , testCase "invalid" $ [] @=? (TTC.readsWithParse :: ReadS PosInt) "-42"
    ]

testReadsEnum :: TestTree
testReadsEnum = testGroup "readsEnum"
    [ testCase "valid" $ Just Red @=? readMaybe "R"
    , testCase "invalid" $ Nothing @=? (readMaybe "bl" :: Maybe Color)
    ]

------------------------------------------------------------------------------
-- $Valid

testValid :: TestTree
testValid = testCase "valid" $ TestString "test" @=? validConst
  where
    validConst :: TestString
    validConst = $$(TTC.valid "test")

testValidIsString :: TestTree
testValidIsString =
    testCase "validIsString" $ TestString "test" @=? validConst
  where
    validConst :: TestString
    validConst = $$("test")

testValidOf :: TestTree
testValidOf = testCase "validOf" $
    TestString "test" @=? $$(TTC.validOf (Proxy :: Proxy TestString) "test")

testMkValid :: TestTree
testMkValid = testCase "mkValid" $
    TestString "test" @=? $$(TestString.valid "test")

testUntypedValidOf :: TestTree
testUntypedValidOf = testCase "untypedValidOf" $
    TestString "test" @=?
      $(TTC.untypedValidOf (Proxy :: Proxy TestString) "test")

testMkUntypedValid :: TestTree
testMkUntypedValid = testCase "mkUntypedValid" $
    TestString "test" @=? $(TestString.untypedValid "test")

testMkUntypedValidQQ :: TestTree
testMkUntypedValidQQ = testCase "mkUntypedValidQQ" $
    TestString "test" @=? [TestString.untypedValidQQ|test|]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Data.TTC"
    [ testGroup "Textual"
        [ testConvert
        , testToS
        , testToT
        , testToTL
        , testToTLB
        , testToST
        , testToBS
        , testToBSL
        , testToBSB
        , testToSBS
        , testFromS
        , testFromT
        , testFromTL
        , testFromTLB
        , testFromBS
        , testFromBSL
        , testFromBSB
        , testFromSBS
        , testAsS
        , testAsT
        , testAsTL
        , testAsTLB
        , testAsBS
        , testAsBSL
        , testAsBSB
        , testAsSBS
        ]
    , testGroup "Render"
        [ testRender
        , testRenderDefault
        , testRenderWithShow
        , testRenderS
        , testRenderT
        , testRenderTL
        , testRenderTLB
        , testRenderST
        , testRenderBS
        , testRenderBSL
        , testRenderBSB
        , testRenderSBS
        ]
    , testGroup "Parse"
        [ testParse
        , testParseDefault
        , testWithError
        , testWithErrorS
        , testWithErrorT
        , testWithErrorTL
        , testWithErrorTLB
        , testWithErrorST
        , testWithErrorBS
        , testWithErrorBSL
        , testWithErrorBSB
        , testWithErrorSBS
        , testPrefixError
        , testPrefixErrorS
        , testPrefixErrorT
        , testPrefixErrorTL
        , testPrefixErrorTLB
        , testPrefixErrorST
        , testPrefixErrorBS
        , testPrefixErrorBSL
        , testPrefixErrorBSB
        , testPrefixErrorSBS
        , testParseWithRead
        , testParseWithRead'
        , testMaybeParseWithRead
        , testParseEnum
        , testParseEnum'
        , testParseS
        , testParseT
        , testParseTL
        , testParseTLB
        , testParseST
        , testParseBS
        , testParseBSL
        , testParseBSB
        , testParseSBS
        , testParseMaybe
        , testParseMaybeS
        , testParseMaybeT
        , testParseMaybeTL
        , testParseMaybeTLB
        , testParseMaybeST
        , testParseMaybeBS
        , testParseMaybeBSL
        , testParseMaybeBSB
        , testParseMaybeSBS
        , testParseOrFail
        , testParseOrFailS
        , testParseOrFailT
        , testParseOrFailTL
        , testParseOrFailTLB
        , testParseOrFailST
        , testParseOrFailBS
        , testParseOrFailBSL
        , testParseOrFailBSB
        , testParseOrFailSBS
        , testParseUnsafe
        , testParseUnsafeS
        , testParseUnsafeT
        , testParseUnsafeTL
        , testParseUnsafeTLB
        , testParseUnsafeST
        , testParseUnsafeBS
        , testParseUnsafeBSL
        , testParseUnsafeBSB
        , testParseUnsafeSBS
        , testReadsWithParse
        , testReadsEnum
        ]
    , testGroup "Valid"
        [ testValid
        , testValidIsString
        , testValidOf
        , testMkValid
        , testUntypedValidOf
        , testMkUntypedValid
        , testMkUntypedValidQQ
        ]
    ]
