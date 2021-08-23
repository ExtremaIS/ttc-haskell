{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TTC.Test (tests) where

-- https://hackage.haskell.org/package/base
import Control.Exception (ErrorCall, Exception, evaluate, handle)
import Control.Monad (when)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.Word (Word16, Word32, Word64, Word8)
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

-- (ttc)
import qualified Data.TTC as TTC

-- (ttc:test)
import qualified TestString
import TestString (TestString(TestString))

------------------------------------------------------------------------------
-- $Instances

instance TTC.Render Char
instance TTC.Parse Char

instance TTC.Render Double
instance TTC.Parse Double

instance TTC.Render Float
instance TTC.Parse Float

instance TTC.Render Int
instance TTC.Parse Int

instance TTC.Render Int8
instance TTC.Parse Int8

instance TTC.Render Int16
instance TTC.Parse Int16

instance TTC.Render Int32
instance TTC.Parse Int32

instance TTC.Render Int64
instance TTC.Parse Int64

instance TTC.Render Integer
instance TTC.Parse Integer

instance TTC.Render Word
instance TTC.Parse Word

instance TTC.Render Word8
instance TTC.Parse Word8

instance TTC.Render Word16
instance TTC.Parse Word16

instance TTC.Render Word32
instance TTC.Parse Word32

instance TTC.Render Word64
instance TTC.Parse Word64

instance TTC.Render String
instance TTC.Parse String

instance TTC.Render BSL.ByteString
instance TTC.Parse BSL.ByteString

instance TTC.Render BS.ByteString
instance TTC.Parse BS.ByteString

instance TTC.Render TL.Text
instance TTC.Parse TL.Text

instance TTC.Render T.Text
instance TTC.Parse T.Text

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
  deriving Eq

instance Show PosInt where
  show (PosInt i) = "(PosInt " ++ show i ++ ")"

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
  render Red   = TTC.fromT "red"
  render Green = TTC.fromT "green"
  render Blue  = TTC.fromT "blue"
  render White = TTC.fromT "white"
  render Black = TTC.fromT "black"

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
  parse = TTC.asS $ \s -> do
    when (null s) $ Left (TTC.fromT undefined)
    pure $ PartialParser s

------------------------------------------------------------------------------
-- $Textual

testToS :: TestTree
testToS = testGroup "toS"
    [ testCase "S" $ xS @=? TTC.toS xS
    , testCase "T" $ xS @=? TTC.toS xT
    , testCase "TL" $ xS @=? TTC.toS xTL
    , testCase "TLB" $ xS @=? TTC.toS xTLB
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
    [ testCase "S" $ xT @=? TTC.toT xS
    , testCase "T" $ xT @=? TTC.toT xT
    , testCase "TL" $ xT @=? TTC.toT xTL
    , testCase "TLB" $ xT @=? TTC.toT xTLB
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
    [ testCase "S" $ xTL @=? TTC.toTL xS
    , testCase "T" $ xTL @=? TTC.toTL xT
    , testCase "TL" $ xTL @=? TTC.toTL xTL
    , testCase "TLB" $ xTL @=? TTC.toTL xTLB
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
    [ testCase "S" $ xTLB @=? TTC.toTLB xS
    , testCase "T" $ xTLB @=? TTC.toTLB xT
    , testCase "TL" $ xTLB @=? TTC.toTLB xTL
    , testCase "TLB" $ xTLB @=? TTC.toTLB xTLB
    , testCase "BS" $ xTLB @=? TTC.toTLB xBS
    , testCase "BS/invalid" $ xiTLB @=? TTC.toTLB xiBS
    , testCase "BSL" $ xTLB @=? TTC.toTLB xBSL
    , testCase "BSL/invalid" $ xiTLB @=? TTC.toTLB xiBSL
    , testCase "BSB" $ xTLB @=? TTC.toTLB xBSB
    , testCase "BSB/invalid" $ xiTLB @=? TTC.toTLB xiBSB
    , testCase "SBS" $ xTLB @=? TTC.toTLB xSBS
    , testCase "SBS/invalid" $ xiTLB @=? TTC.toTLB xiSBS
    ]

testToBS :: TestTree
testToBS = testGroup "toBS"
    [ testCase "S" $ xBS @=? TTC.toBS xS
    , testCase "T" $ xBS @=? TTC.toBS xT
    , testCase "TL" $ xBS @=? TTC.toBS xTL
    , testCase "TLB" $ xBS @=? TTC.toBS xTLB
    , testCase "BS" $ xBS @=? TTC.toBS xBS
    , testCase "BSL" $ xBS @=? TTC.toBS xBSL
    , testCase "BSB" $ xBS @=? TTC.toBS xBSB
    , testCase "SBS" $ xBS @=? TTC.toBS xSBS
    ]

testToBSL :: TestTree
testToBSL = testGroup "toBSL"
    [ testCase "S" $ xBSL @=? TTC.toBSL xS
    , testCase "T" $ xBSL @=? TTC.toBSL xT
    , testCase "TL" $ xBSL @=? TTC.toBSL xTL
    , testCase "TLB" $ xBSL @=? TTC.toBSL xTLB
    , testCase "BS" $ xBSL @=? TTC.toBSL xBS
    , testCase "BSL" $ xBSL @=? TTC.toBSL xBSL
    , testCase "BSB" $ xBSL @=? TTC.toBSL xBSB
    , testCase "SBS" $ xBSL @=? TTC.toBSL xSBS
    ]

testToBSB :: TestTree
testToBSB = testGroup "toBSB"
    [ testCase "S" $ xBSB @=? TTC.toBSB xS
    , testCase "T" $ xBSB @=? TTC.toBSB xT
    , testCase "TL" $ xBSB @=? TTC.toBSB xTL
    , testCase "TLB" $ xBSB @=? TTC.toBSB xTLB
    , testCase "BS" $ xBSB @=? TTC.toBSB xBS
    , testCase "BSL" $ xBSB @=? TTC.toBSB xBSL
    , testCase "BSB" $ xBSB @=? TTC.toBSB xBSB
    , testCase "SBS" $ xBSB @=? TTC.toBSB xSBS
    ]

testToSBS :: TestTree
testToSBS = testGroup "toSBS"
    [ testCase "S" $ xSBS @=? TTC.toSBS xS
    , testCase "T" $ xSBS @=? TTC.toSBS xT
    , testCase "TL" $ xSBS @=? TTC.toSBS xTL
    , testCase "TLB" $ xSBS @=? TTC.toSBS xTLB
    , testCase "BS" $ xSBS @=? TTC.toSBS xBS
    , testCase "BSL" $ xSBS @=? TTC.toSBS xBSL
    , testCase "BSB" $ xSBS @=? TTC.toSBS xBSB
    , testCase "SBS" $ xSBS @=? TTC.toSBS xSBS
    ]

testConvert :: TestTree
testConvert = testGroup "convert"
    [ testCase "S->S" $ xS @=? TTC.convert xS
    , testCase "S->T" $ xT @=? TTC.convert xS
    , testCase "S->TL" $ xTL @=? TTC.convert xS
    , testCase "S->TLB" $ xTLB @=? TTC.convert xS
    , testCase "S->BS" $ xBS @=? TTC.convert xS
    , testCase "S->BSL" $ xBSL @=? TTC.convert xS
    , testCase "S->BSB" $ xBSB @=? TTC.convert xS
    , testCase "S->SBS" $ xSBS @=? TTC.convert xS
    , testCase "T->S" $ xS @=? TTC.convert xT
    , testCase "T->T" $ xT @=? TTC.convert xT
    , testCase "T->TL" $ xTL @=? TTC.convert xT
    , testCase "T->TLB" $ xTLB @=? TTC.convert xT
    , testCase "T->BS" $ xBS @=? TTC.convert xT
    , testCase "T->BSL" $ xBSL @=? TTC.convert xT
    , testCase "T->BSB" $ xBSB @=? TTC.convert xT
    , testCase "T->SBS" $ xSBS @=? TTC.convert xT
    , testCase "TL->S" $ xS @=? TTC.convert xTL
    , testCase "TL->T" $ xT @=? TTC.convert xTL
    , testCase "TL->TL" $ xTL @=? TTC.convert xTL
    , testCase "TL->TLB" $ xTLB @=? TTC.convert xTL
    , testCase "TL->BS" $ xBS @=? TTC.convert xTL
    , testCase "TL->BSL" $ xBSL @=? TTC.convert xTL
    , testCase "TL->BSB" $ xBSB @=? TTC.convert xTL
    , testCase "TL->SBS" $ xSBS @=? TTC.convert xTL
    , testCase "TLB->S" $ xS @=? TTC.convert xTLB
    , testCase "TLB->T" $ xT @=? TTC.convert xTLB
    , testCase "TLB->TL" $ xTL @=? TTC.convert xTLB
    , testCase "TLB->TLB" $ xTLB @=? TTC.convert xTLB
    , testCase "TLB->BS" $ xBS @=? TTC.convert xTLB
    , testCase "TLB->BSL" $ xBSL @=? TTC.convert xTLB
    , testCase "TLB->BSB" $ xBSB @=? TTC.convert xTLB
    , testCase "TLB->SBS" $ xSBS @=? TTC.convert xTLB
    , testCase "BS->S" $ xS @=? TTC.convert xBS
    , testCase "BS->T" $ xT @=? TTC.convert xBS
    , testCase "BS->TL" $ xTL @=? TTC.convert xBS
    , testCase "BS->TLB" $ xTLB @=? TTC.convert xBS
    , testCase "BS->BS" $ xBS @=? TTC.convert xBS
    , testCase "BS->BSL" $ xBSL @=? TTC.convert xBS
    , testCase "BS->BSB" $ xBSB @=? TTC.convert xBS
    , testCase "BS->SBS" $ xSBS @=? TTC.convert xBS
    , testCase "BSL->S" $ xS @=? TTC.convert xBSL
    , testCase "BSL->T" $ xT @=? TTC.convert xBSL
    , testCase "BSL->TL" $ xTL @=? TTC.convert xBSL
    , testCase "BSL->TLB" $ xTLB @=? TTC.convert xBSL
    , testCase "BSL->BS" $ xBS @=? TTC.convert xBSL
    , testCase "BSL->BSL" $ xBSL @=? TTC.convert xBSL
    , testCase "BSL->BSB" $ xBSB @=? TTC.convert xBSL
    , testCase "BSL->SBS" $ xSBS @=? TTC.convert xBSL
    , testCase "BSB->S" $ xS @=? TTC.convert xBSB
    , testCase "BSB->T" $ xT @=? TTC.convert xBSB
    , testCase "BSB->TL" $ xTL @=? TTC.convert xBSB
    , testCase "BSB->TLB" $ xTLB @=? TTC.convert xBSB
    , testCase "BSB->BS" $ xBS @=? TTC.convert xBSB
    , testCase "BSB->BSL" $ xBSL @=? TTC.convert xBSB
    , testCase "BSB->BSB" $ xBSB @=? TTC.convert xBSB
    , testCase "BSB->SBS" $ xSBS @=? TTC.convert xBSB
    , testCase "SBS->S" $ xS @=? TTC.convert xSBS
    , testCase "SBS->T" $ xT @=? TTC.convert xSBS
    , testCase "SBS->TL" $ xTL @=? TTC.convert xSBS
    , testCase "SBS->TLB" $ xTLB @=? TTC.convert xSBS
    , testCase "SBS->BS" $ xBS @=? TTC.convert xSBS
    , testCase "SBS->BSL" $ xBSL @=? TTC.convert xSBS
    , testCase "SBS->BSB" $ xBSB @=? TTC.convert xSBS
    , testCase "SBS->SBS" $ xSBS @=? TTC.convert xSBS
    ]

testFromS :: TestTree
testFromS = testGroup "fromS"
    [ testCase "S" $ xS @=? TTC.fromS xS
    , testCase "T" $ xT @=? TTC.fromS xS
    , testCase "TL" $ xTL @=? TTC.fromS xS
    , testCase "TLB" $ xTLB @=? TTC.fromS xS
    , testCase "BS" $ xBS @=? TTC.fromS xS
    , testCase "BSL" $ xBSL @=? TTC.fromS xS
    , testCase "BSB" $ xBSB @=? TTC.fromS xS
    , testCase "SBS" $ xSBS @=? TTC.fromS xS
    ]

testFromT :: TestTree
testFromT = testGroup "fromT"
    [ testCase "S" $ xS @=? TTC.fromT xT
    , testCase "T" $ xT @=? TTC.fromT xT
    , testCase "TL" $ xTL @=? TTC.fromT xT
    , testCase "TLB" $ xTLB @=? TTC.fromT xT
    , testCase "BS" $ xBS @=? TTC.fromT xT
    , testCase "BSL" $ xBSL @=? TTC.fromT xT
    , testCase "BSB" $ xBSB @=? TTC.fromT xT
    , testCase "SBS" $ xSBS @=? TTC.fromT xT
    ]

testFromTL :: TestTree
testFromTL = testGroup "fromTL"
    [ testCase "S" $ xS @=? TTC.fromTL xTL
    , testCase "T" $ xT @=? TTC.fromTL xTL
    , testCase "TL" $ xTL @=? TTC.fromTL xTL
    , testCase "TLB" $ xTLB @=? TTC.fromTL xTL
    , testCase "BS" $ xBS @=? TTC.fromTL xTL
    , testCase "BSL" $ xBSL @=? TTC.fromTL xTL
    , testCase "BSB" $ xBSB @=? TTC.fromTL xTL
    , testCase "SBS" $ xSBS @=? TTC.fromTL xTL
    ]

testFromTLB :: TestTree
testFromTLB = testGroup "fromTLB"
    [ testCase "S" $ xS @=? TTC.fromTLB xTLB
    , testCase "T" $ xT @=? TTC.fromTLB xTLB
    , testCase "TL" $ xTL @=? TTC.fromTLB xTLB
    , testCase "TLB" $ xTLB @=? TTC.fromTLB xTLB
    , testCase "BS" $ xBS @=? TTC.fromTLB xTLB
    , testCase "BSL" $ xBSL @=? TTC.fromTLB xTLB
    , testCase "BSB" $ xBSB @=? TTC.fromTLB xTLB
    , testCase "SBS" $ xSBS @=? TTC.fromTLB xTLB
    ]

testFromBS :: TestTree
testFromBS = testGroup "fromBS"
    [ testCase "S" $ xS @=? TTC.fromBS xBS
    , testCase "T" $ xT @=? TTC.fromBS xBS
    , testCase "TL" $ xTL @=? TTC.fromBS xBS
    , testCase "TLB" $ xTLB @=? TTC.fromBS xBS
    , testCase "BS" $ xBS @=? TTC.fromBS xBS
    , testCase "BSL" $ xBSL @=? TTC.fromBS xBS
    , testCase "BSB" $ xBSB @=? TTC.fromBS xBS
    , testCase "SBS" $ xSBS @=? TTC.fromBS xBS
    ]

testFromBSL :: TestTree
testFromBSL = testGroup "fromBSL"
    [ testCase "S" $ xS @=? TTC.fromBSL xBSL
    , testCase "T" $ xT @=? TTC.fromBSL xBSL
    , testCase "TL" $ xTL @=? TTC.fromBSL xBSL
    , testCase "TLB" $ xTLB @=? TTC.fromBSL xBSL
    , testCase "BS" $ xBS @=? TTC.fromBSL xBSL
    , testCase "BSL" $ xBSL @=? TTC.fromBSL xBSL
    , testCase "BSB" $ xBSB @=? TTC.fromBSL xBSL
    , testCase "SBS" $ xSBS @=? TTC.fromBSL xBSL
    ]

testFromBSB :: TestTree
testFromBSB = testGroup "fromBSB"
    [ testCase "S" $ xS @=? TTC.fromBSB xBSB
    , testCase "T" $ xT @=? TTC.fromBSB xBSB
    , testCase "TL" $ xTL @=? TTC.fromBSB xBSB
    , testCase "TLB" $ xTLB @=? TTC.fromBSB xBSB
    , testCase "BS" $ xBS @=? TTC.fromBSB xBSB
    , testCase "BSL" $ xBSL @=? TTC.fromBSB xBSB
    , testCase "BSB" $ xBSB @=? TTC.fromBSB xBSB
    , testCase "SBS" $ xSBS @=? TTC.fromBSB xBSB
    ]

testFromSBS :: TestTree
testFromSBS = testGroup "fromSBS"
    [ testCase "S" $ xS @=? TTC.fromSBS xSBS
    , testCase "T" $ xT @=? TTC.fromSBS xSBS
    , testCase "TL" $ xTL @=? TTC.fromSBS xSBS
    , testCase "TLB" $ xTLB @=? TTC.fromSBS xSBS
    , testCase "BS" $ xBS @=? TTC.fromSBS xSBS
    , testCase "BSL" $ xBSL @=? TTC.fromSBS xSBS
    , testCase "BSB" $ xBSB @=? TTC.fromSBS xSBS
    , testCase "SBS" $ xSBS @=? TTC.fromSBS xSBS
    ]

testAsS :: TestTree
testAsS = testGroup "asS"
    [ testCase "S" $ xS @=? TTC.asS id xS
    , testCase "T" $ xS @=? TTC.asS id xT
    , testCase "TL" $ xS @=? TTC.asS id xTL
    , testCase "TLB" $ xS @=? TTC.asS id xTLB
    , testCase "BS" $ xS @=? TTC.asS id xBS
    , testCase "BSL" $ xS @=? TTC.asS id xBSL
    , testCase "BSB" $ xS @=? TTC.asS id xBSB
    , testCase "SBS" $ xS @=? TTC.asS id xSBS
    ]

testAsT :: TestTree
testAsT = testGroup "asT"
    [ testCase "S" $ xT @=? TTC.asT id xS
    , testCase "T" $ xT @=? TTC.asT id xT
    , testCase "TL" $ xT @=? TTC.asT id xTL
    , testCase "TLB" $ xT @=? TTC.asT id xTLB
    , testCase "BS" $ xT @=? TTC.asT id xBS
    , testCase "BSL" $ xT @=? TTC.asT id xBSL
    , testCase "BSB" $ xT @=? TTC.asT id xBSB
    , testCase "SBS" $ xT @=? TTC.asT id xSBS
    ]

testAsTL :: TestTree
testAsTL = testGroup "asTL"
    [ testCase "S" $ xTL @=? TTC.asTL id xS
    , testCase "T" $ xTL @=? TTC.asTL id xT
    , testCase "TL" $ xTL @=? TTC.asTL id xTL
    , testCase "TLB" $ xTL @=? TTC.asTL id xTLB
    , testCase "BS" $ xTL @=? TTC.asTL id xBS
    , testCase "BSL" $ xTL @=? TTC.asTL id xBSL
    , testCase "BSB" $ xTL @=? TTC.asTL id xBSB
    , testCase "SBS" $ xTL @=? TTC.asTL id xSBS
    ]

testAsTLB :: TestTree
testAsTLB = testGroup "asTLB"
    [ testCase "S" $ xTLB @=? TTC.asTLB id xS
    , testCase "T" $ xTLB @=? TTC.asTLB id xT
    , testCase "TL" $ xTLB @=? TTC.asTLB id xTL
    , testCase "TLB" $ xTLB @=? TTC.asTLB id xTLB
    , testCase "BS" $ xTLB @=? TTC.asTLB id xBS
    , testCase "BSL" $ xTLB @=? TTC.asTLB id xBSL
    , testCase "BSB" $ xTLB @=? TTC.asTLB id xBSB
    , testCase "SBS" $ xTLB @=? TTC.asTLB id xSBS
    ]

testAsBS :: TestTree
testAsBS = testGroup "asBS"
    [ testCase "S" $ xBS @=? TTC.asBS id xS
    , testCase "T" $ xBS @=? TTC.asBS id xT
    , testCase "TL" $ xBS @=? TTC.asBS id xTL
    , testCase "TLB" $ xBS @=? TTC.asBS id xTLB
    , testCase "BS" $ xBS @=? TTC.asBS id xBS
    , testCase "BSL" $ xBS @=? TTC.asBS id xBSL
    , testCase "BSB" $ xBS @=? TTC.asBS id xBSB
    , testCase "SBS" $ xBS @=? TTC.asBS id xSBS
    ]

testAsBSL :: TestTree
testAsBSL = testGroup "asBSL"
    [ testCase "S" $ xBSL @=? TTC.asBSL id xS
    , testCase "T" $ xBSL @=? TTC.asBSL id xT
    , testCase "TL" $ xBSL @=? TTC.asBSL id xTL
    , testCase "TLB" $ xBSL @=? TTC.asBSL id xTLB
    , testCase "BS" $ xBSL @=? TTC.asBSL id xBS
    , testCase "BSL" $ xBSL @=? TTC.asBSL id xBSL
    , testCase "BSB" $ xBSL @=? TTC.asBSL id xBSB
    , testCase "SBS" $ xBSL @=? TTC.asBSL id xSBS
    ]

testAsBSB :: TestTree
testAsBSB = testGroup "asBSB"
    [ testCase "S" $ xBSB @=? TTC.asBSB id xS
    , testCase "T" $ xBSB @=? TTC.asBSB id xT
    , testCase "TL" $ xBSB @=? TTC.asBSB id xTL
    , testCase "TLB" $ xBSB @=? TTC.asBSB id xTLB
    , testCase "BS" $ xBSB @=? TTC.asBSB id xBS
    , testCase "BSL" $ xBSB @=? TTC.asBSB id xBSL
    , testCase "BSB" $ xBSB @=? TTC.asBSB id xBSB
    , testCase "SBS" $ xBSB @=? TTC.asBSB id xSBS
    ]

testAsSBS :: TestTree
testAsSBS = testGroup "asSBS"
    [ testCase "S" $ xSBS @=? TTC.asSBS id xS
    , testCase "T" $ xSBS @=? TTC.asSBS id xT
    , testCase "TL" $ xSBS @=? TTC.asSBS id xTL
    , testCase "TLB" $ xSBS @=? TTC.asSBS id xTLB
    , testCase "BS" $ xSBS @=? TTC.asSBS id xBS
    , testCase "BSL" $ xSBS @=? TTC.asSBS id xBSL
    , testCase "BSB" $ xSBS @=? TTC.asSBS id xBSB
    , testCase "SBS" $ xSBS @=? TTC.asSBS id xSBS
    ]

------------------------------------------------------------------------------
-- $Render

testRender :: TestTree
testRender = testGroup "render"
    [ testCase "S" $ answerS @=? TTC.render answer
    , testCase "T" $ answerT @=? TTC.render answer
    , testCase "TL" $ answerTL @=? TTC.render answer
    , testCase "TLB" $ answerTLB @=? TTC.render answer
    , testCase "BS" $ answerBS @=? TTC.render answer
    , testCase "BSL" $ answerBSL @=? TTC.render answer
    , testCase "BSB" $ answerBSB @=? TTC.render answer
    , testCase "SBS" $ answerSBS @=? TTC.render answer
    ]

testRenderDefault :: TestTree
testRenderDefault = testGroup "RenderDefault"
    [ testCase "Char" $ "*" @=? TTC.renderS '*'
    , let x = 3.14159 :: Double
      in  testCase "Double" $ show x @=? TTC.renderS x
    , let x = 3.14159 :: Float
      in  testCase "Float" $ show x @=? TTC.renderS x
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
    , let n = 42 :: Integer
      in  testCase "Integer" $ show n @=? TTC.renderS n
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
    , testCase "BSL.ByteString" $ xS @=? TTC.renderS xBSL
    , testCase "BS.ByteString" $ xS @=? TTC.renderS xBS
    , testCase "TL.Text" $ xS @=? TTC.renderS xTL
    , testCase "T.Text" $ xS @=? TTC.renderS xT
    ]

testRenderS :: TestTree
testRenderS = testCase "renderS" $ answerS @=? TTC.renderS answer

testRenderT :: TestTree
testRenderT = testCase "renderT" $ answerT @=? TTC.renderT answer

testRenderTL :: TestTree
testRenderTL = testCase "renderTL" $ answerTL @=? TTC.renderTL answer

testRenderTLB :: TestTree
testRenderTLB = testCase "renderTLB" $ answerTLB @=? TTC.renderTLB answer

testRenderBS :: TestTree
testRenderBS = testCase "renderBS" $ answerBS @=? TTC.renderBS answer

testRenderBSL :: TestTree
testRenderBSL = testCase "renderBSL" $ answerBSL @=? TTC.renderBSL answer

testRenderBSB :: TestTree
testRenderBSB = testCase "renderBSB" $
    answerBSL @=? BSB.toLazyByteString (TTC.renderBSB answer)

testRenderSBS :: TestTree
testRenderSBS = testCase "renderSBS" $ answerSBS @=? TTC.renderSBS answer

testRenderWithShow :: TestTree
testRenderWithShow = testGroup "renderWithShow"
    [ testCase "S" $ answerS @=? TTC.renderWithShow answerZ
    , testCase "T" $ answerT @=? TTC.renderWithShow answerZ
    , testCase "TL" $ answerTL @=? TTC.renderWithShow answerZ
    , testCase "TLB" $ answerTLB @=? TTC.renderWithShow answerZ
    , testCase "BS" $ answerBS @=? TTC.renderWithShow answerZ
    , testCase "BSL" $ answerBSL @=? TTC.renderWithShow answerZ
    , testCase "BSB" $ answerBSB @=? TTC.renderWithShow answerZ
    , testCase "SBS" $ answerSBS @=? TTC.renderWithShow answerZ
    ]

------------------------------------------------------------------------------
-- $Parse

testParse :: TestTree
testParse = testGroup "parse"
    [ testCase "S" $ Just answer @=? TTC.parseMaybe answerS
    , testCase "T" $ Just answer @=? TTC.parseMaybe answerT
    , testCase "TL" $ Just answer @=? TTC.parseMaybe answerTL
    , testCase "TLB" $ Just answer @=? TTC.parseMaybe answerTLB
    , testCase "BS" $ Just answer @=? TTC.parseMaybe answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseMaybe answerBSL
    , testCase "BSB" $ Just answer @=? TTC.parseMaybe answerBSB
    , testCase "SBS" $ Just answer @=? TTC.parseMaybe answerSBS
    , testCase "negative" $ Left "not positive" @=?
        (TTC.parse ('-' : answerS) :: Either String PosInt)
    , testCase "invalid" $ Left "not an integer" @=?
        (TTC.parse ('a' : answerS) :: Either String PosInt)
    ]

testParseDefault :: TestTree
testParseDefault = testGroup "ParseDefault"
    [ let parse = TTC.parse :: String -> Either String Char
      in  testGroup "Char"
            [ testCase "OK" $ Right '*' @=? parse "*"
            , testCase "empty" $ Left "invalid Char" @=? parse ""
            , testCase "multiple" $ Left "invalid Char" @=? parse "**"
            ]
    , let parse = TTC.parse :: String -> Either String Double
          s = show (3.14159 :: Double)
      in  testGroup "Double"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Double" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Float
          s = show (3.14159 :: Float)
      in  testGroup "Float"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Float" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Int
          s = show (42 :: Int)
      in  testGroup "Int"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Int" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Int8
          s = show (42 :: Int8)
      in  testGroup "Int8"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Int8" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Int16
          s = show (42 :: Int16)
      in  testGroup "Int16"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Int16" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Int32
          s = show (42 :: Int32)
      in  testGroup "Int32"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Int32" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Int64
          s = show (42 :: Int64)
      in  testGroup "Int64"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Int64" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Integer
          s = show (42 :: Integer)
      in  testGroup "Integer"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Integer" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Word
          s = show (42 :: Word)
      in  testGroup "Word"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Word" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Word8
          s = show (42 :: Word8)
      in  testGroup "Word8"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Word8" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Word16
          s = show (42 :: Word16)
      in  testGroup "Word16"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Word16" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Word32
          s = show (42 :: Word32)
      in  testGroup "Word32"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Word32" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String Word64
          s = show (42 :: Word64)
      in  testGroup "Word64"
            [ testCase "OK" $ Right (read s) @=? parse s
            , testCase "invalid" $ Left "invalid Word64" @=? parse "invalid"
            ]
    , let parse = TTC.parse :: String -> Either String String
      in  testGroup "String"
            [ testCase "empty" $ Right "" @=? parse ""
            , testCase "nonempty" $ Right xS @=? parse xS
            ]
    , let parse = TTC.parse :: String -> Either String BSL.ByteString
      in  testGroup "BSL.ByteString"
            [ testCase "empty" $ Right BSL.empty @=? parse ""
            , testCase "nonempty" $ Right xBSL @=? parse xS
            ]
    , let parse = TTC.parse :: String -> Either String BS.ByteString
      in  testGroup "BS.ByteString"
            [ testCase "empty" $ Right BS.empty @=? parse ""
            , testCase "nonempty" $ Right xBS @=? parse xS
            ]
    , let parse = TTC.parse :: String -> Either String TL.Text
      in  testGroup "TL.Text"
            [ testCase "empty" $ Right TL.empty @=? parse ""
            , testCase "nonempty" $ Right xTL @=? parse xS
            ]
    , let parse = TTC.parse :: String -> Either String T.Text
      in  testGroup "T.Text"
            [ testCase "empty" $ Right T.empty @=? parse ""
            , testCase "nonempty" $ Right xT @=? parse xS
            ]
    ]

testParseS :: TestTree
testParseS =
    let parseS = TTC.parseS :: String -> Either String PosInt
    in  testCase "parseS" $ Right answer @=? parseS answerS

testParseT :: TestTree
testParseT =
    let parseT = TTC.parseT :: T.Text -> Either String PosInt
    in  testCase "parseT" $ Right answer @=? parseT answerT

testParseTL :: TestTree
testParseTL =
    let parseTL = TTC.parseTL :: TL.Text -> Either String PosInt
    in  testCase "parseTL" $ Right answer @=? parseTL answerTL

testParseTLB :: TestTree
testParseTLB =
    let parseTLB = TTC.parseTLB :: TLB.Builder -> Either String PosInt
    in  testCase "parseTLB" $ Right answer @=? parseTLB answerTLB

testParseBS :: TestTree
testParseBS =
    let parseBS = TTC.parseBS :: BS.ByteString -> Either String PosInt
    in  testCase "parseBS" $ Right answer @=? parseBS answerBS

testParseBSL :: TestTree
testParseBSL =
    let parseBSL = TTC.parseBSL :: BSL.ByteString -> Either String PosInt
    in  testCase "parseBSL" $ Right answer @=? parseBSL answerBSL

testParseBSB :: TestTree
testParseBSB =
    let parseBSB = TTC.parseBSB :: BSB.Builder -> Either String PosInt
    in  testCase "parseBSB" $ Right answer @=? parseBSB answerBSB

testParseSBS :: TestTree
testParseSBS =
    let parseSBS = TTC.parseSBS :: SBS.ShortByteString -> Either String PosInt
    in  testCase "parseSBS" $ Right answer @=? parseSBS answerSBS

testParseMaybe :: TestTree
testParseMaybe = testGroup "parseMaybe"
    [ testCase "S" $ Just answer @=? TTC.parseMaybe answerS
    , testCase "T" $ Just answer @=? TTC.parseMaybe answerT
    , testCase "TL" $ Just answer @=? TTC.parseMaybe answerTL
    , testCase "TLB" $ Just answer @=? TTC.parseMaybe answerTLB
    , testCase "BS" $ Just answer @=? TTC.parseMaybe answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseMaybe answerBSL
    , testCase "BSB" $ Just answer @=? TTC.parseMaybe answerBSB
    , testCase "SBS" $ Just answer @=? TTC.parseMaybe answerSBS
    -- successful 'parseMaybe' does not have any error type conversion:
    , testCase "noerror" $
        Just (PartialParser "test") @=? TTC.parseMaybe ("test" :: String)
    , testCase "negative" $
        Nothing @=? (TTC.parseMaybe ('-' : answerS) :: Maybe PosInt)
    , testCase "invalid" $
        Nothing @=? (TTC.parseMaybe ('a' : answerS) :: Maybe PosInt)
    ]

testParseMaybeS :: TestTree
testParseMaybeS = testCase "parseMaybeS" $
    Just answer @=? TTC.parseMaybeS answerS

testParseMaybeT :: TestTree
testParseMaybeT = testCase "parseMaybeT" $
    Just answer @=? TTC.parseMaybeT answerT

testParseMaybeTL :: TestTree
testParseMaybeTL = testCase "parseMaybeTL" $
    Just answer @=? TTC.parseMaybeTL answerTL

testParseMaybeTLB :: TestTree
testParseMaybeTLB = testCase "parseMaybeTLB" $
    Just answer @=? TTC.parseMaybeTLB answerTLB

testParseMaybeBS :: TestTree
testParseMaybeBS = testCase "parseMaybeBS" $
    Just answer @=? TTC.parseMaybeBS answerBS

testParseMaybeBSL :: TestTree
testParseMaybeBSL = testCase "parseMaybeBSL" $
    Just answer @=? TTC.parseMaybeBSL answerBSL

testParseMaybeBSB :: TestTree
testParseMaybeBSB = testCase "parseMaybeBSB" $
    Just answer @=? TTC.parseMaybeBSB answerBSB

testParseMaybeSBS :: TestTree
testParseMaybeSBS = testCase "parseMaybeSBS" $
    Just answer @=? TTC.parseMaybeSBS answerSBS

testParseUnsafe :: TestTree
testParseUnsafe = testGroup "parseUnsafe"
    [ testCase "S" $ answer @=? TTC.parseUnsafe answerS
    , testCase "T" $ answer @=? TTC.parseUnsafe answerT
    , testCase "TL" $ answer @=? TTC.parseUnsafe answerTL
    , testCase "TLB" $ answer @=? TTC.parseUnsafe answerTLB
    , testCase "BS" $ answer @=? TTC.parseUnsafe answerBS
    , testCase "BSL" $ answer @=? TTC.parseUnsafe answerBSL
    , testCase "BSB" $ answer @=? TTC.parseUnsafe answerBSB
    , testCase "SBS" $ answer @=? TTC.parseUnsafe answerSBS
    , testCase "negative" $ assertRaises (Proxy :: Proxy ErrorCall)
        (TTC.parseUnsafe ('-' : answerS) :: PosInt)
    , testCase "invalid" $ assertRaises (Proxy :: Proxy ErrorCall)
        (TTC.parseUnsafe ('a' : answerS) :: PosInt)
    ]

testParseUnsafeS :: TestTree
testParseUnsafeS = testCase "parseUnsafeS" $
    answer @=? TTC.parseUnsafeS answerS

testParseUnsafeT :: TestTree
testParseUnsafeT = testCase "parseUnsafeT" $
    answer @=? TTC.parseUnsafeT answerT

testParseUnsafeTL :: TestTree
testParseUnsafeTL = testCase "parseUnsafeTL" $
    answer @=? TTC.parseUnsafeTL answerTL

testParseUnsafeTLB :: TestTree
testParseUnsafeTLB = testCase "parseUnsafeTLB" $
    answer @=? TTC.parseUnsafeTLB answerTLB

testParseUnsafeBS :: TestTree
testParseUnsafeBS = testCase "parseUnsafeBS" $
    answer @=? TTC.parseUnsafeBS answerBS

testParseUnsafeBSL :: TestTree
testParseUnsafeBSL = testCase "parseUnsafeBSL" $
    answer @=? TTC.parseUnsafeBSL answerBSL

testParseUnsafeBSB :: TestTree
testParseUnsafeBSB = testCase "parseUnsafeBSB" $
    answer @=? TTC.parseUnsafeBSB answerBSB

testParseUnsafeSBS :: TestTree
testParseUnsafeSBS = testCase "parseUnsafeSBS" $
    answer @=? TTC.parseUnsafeSBS answerSBS

testParseWithRead :: TestTree
testParseWithRead = testGroup "parseWithRead"
    [ testCase "S" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerS
    , testCase "T" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerT
    , testCase "TL" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerTL
    , testCase "TLB" $
        Right answerZ @=? TTC.parseWithRead IntInvalid answerTLB
    , testCase "BS" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerBS
    , testCase "BSL" $
        Right answerZ @=? TTC.parseWithRead IntInvalid answerBSL
    , testCase "BSB" $
        Right answerZ @=? TTC.parseWithRead IntInvalid answerBSB
    , testCase "SBS" $
        Right answerZ @=? TTC.parseWithRead IntInvalid answerSBS
    , testCase "invalid" $ Left IntInvalid @=?
        (TTC.parseWithRead IntInvalid ('a' : answerS) :: Either IntError Int)
    ]

testParseWithRead' :: TestTree
testParseWithRead' = testGroup "parseWithRead'"
    [ testCase "S" $ Right answerZ @=? parseWithRead' answerS
    , testCase "T" $ Right answerZ @=? parseWithRead' answerT
    , testCase "TL" $ Right answerZ @=? parseWithRead' answerTL
    , testCase "TLB" $ Right answerZ @=? parseWithRead' answerTLB
    , testCase "BS" $ Right answerZ @=? parseWithRead' answerBS
    , testCase "BSL" $ Right answerZ @=? parseWithRead' answerBSL
    , testCase "BSB" $ Right answerZ @=? parseWithRead' answerBSB
    , testCase "SBS" $ Right answerZ @=? parseWithRead' answerSBS
    , testCase "invalid" $
        Left "invalid Int" @=? parseWithRead' ('a' : answerS)
    ]
  where
    parseWithRead' :: TTC.Textual t => t -> Either String Int
    parseWithRead' = TTC.parseWithRead' "Int"

testMaybeParseWithRead :: TestTree
testMaybeParseWithRead = testGroup "maybeParseWithRead"
    [ testCase "S" $ Just answerZ @=? TTC.maybeParseWithRead answerS
    , testCase "T" $ Just answerZ @=? TTC.maybeParseWithRead answerT
    , testCase "TL" $ Just answerZ @=? TTC.maybeParseWithRead answerTL
    , testCase "TLB" $ Just answerZ @=? TTC.maybeParseWithRead answerTLB
    , testCase "BS" $ Just answerZ @=? TTC.maybeParseWithRead answerBS
    , testCase "BSL" $ Just answerZ @=? TTC.maybeParseWithRead answerBSL
    , testCase "BSB" $ Just answerZ @=? TTC.maybeParseWithRead answerBSB
    , testCase "SBS" $ Just answerZ @=? TTC.maybeParseWithRead answerSBS
    , testCase "invalid" $ Nothing @=?
        (TTC.maybeParseWithRead ('a' : answerS) :: Maybe Int)
    ]

testParseEnum :: TestTree
testParseEnum = testGroup "parseEnum"
    [ testCase "S" $ Right Red @=? parse False False redS
    , testCase "T" $ Right Red @=? parse False False redT
    , testCase "TL" $ Right Red @=? parse False False redTL
    , testCase "TLB" $ Right Red @=? parse False False redTLB
    , testCase "BS" $ Right Red @=? parse False False redBS
    , testCase "BSL" $ Right Red @=? parse False False redBSL
    , testCase "BSB" $ Right Red @=? parse False False redBSB
    , testCase "SBS" $ Right Red @=? parse False False redSBS
    , testCase "CI" $ Right Red @=? parse True False ("Red" :: String)
    , testCase "!CI" $ Left ColorInvalid @=?
        parse False False ("Red" :: String)
    , testCase "prefix" $ Right Red @=? parse False True ("r" :: String)
    , testCase "ambiguous" $ Left ColorAmbiguous @=?
        parse False True ("bl" :: String)
    ]
  where
    parse :: TTC.Textual t => Bool -> Bool -> t -> Either ColorError Color
    parse allowCI allowPrefix =
      TTC.parseEnum allowCI allowPrefix ColorInvalid ColorAmbiguous

testParseEnum' :: TestTree
testParseEnum' = testGroup "parseEnum'"
    [ testCase "S" $ Right Red @=? parse False False redS
    , testCase "T" $ Right Red @=? parse False False redT
    , testCase "TL" $ Right Red @=? parse False False redTL
    , testCase "TLB" $ Right Red @=? parse False False redTLB
    , testCase "BS" $ Right Red @=? parse False False redBS
    , testCase "BSL" $ Right Red @=? parse False False redBSL
    , testCase "BSB" $ Right Red @=? parse False False redBSB
    , testCase "SBS" $ Right Red @=? parse False False redSBS
    , testCase "CI" $ Right Red @=? parse True False ("Red" :: String)
    , testCase "!CI" $ Left "invalid Color" @=?
        parse False False ("Red" :: String)
    , testCase "prefix" $ Right Red @=? parse False True ("r" :: String)
    , testCase "ambiguous" $ Left "ambiguous Color" @=?
        parse False True ("bl" :: String)
    ]
  where
    parse :: TTC.Textual t => Bool -> Bool -> t -> Either String Color
    parse = TTC.parseEnum' "Color"

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
        [ testToS
        , testToT
        , testToTL
        , testToTLB
        , testToBS
        , testToBSL
        , testToBSB
        , testToSBS
        , testConvert
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
        , testRenderS
        , testRenderT
        , testRenderTL
        , testRenderTLB
        , testRenderBS
        , testRenderBSL
        , testRenderBSB
        , testRenderSBS
        , testRenderWithShow
        ]
    , testGroup "Parse"
        [ testParse
        , testParseDefault
        , testParseS
        , testParseT
        , testParseTL
        , testParseTLB
        , testParseBS
        , testParseBSL
        , testParseBSB
        , testParseSBS
        , testParseMaybe
        , testParseMaybeS
        , testParseMaybeT
        , testParseMaybeTL
        , testParseMaybeTLB
        , testParseMaybeBS
        , testParseMaybeBSL
        , testParseMaybeBSB
        , testParseMaybeSBS
        , testParseUnsafe
        , testParseUnsafeS
        , testParseUnsafeT
        , testParseUnsafeTL
        , testParseUnsafeTLB
        , testParseUnsafeBS
        , testParseUnsafeBSL
        , testParseUnsafeBSB
        , testParseUnsafeSBS
        , testParseWithRead
        , testParseWithRead'
        , testMaybeParseWithRead
        , testParseEnum
        , testParseEnum'
        , testReadsWithParse
        , testReadsEnum
        ]
    , testGroup "Valid"
        [ testValid
        , testValidOf
        , testMkValid
        , testUntypedValidOf
        , testMkUntypedValid
        , testMkUntypedValidQQ
        ]
    ]
