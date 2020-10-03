{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.TTC.Test (tests) where

-- https://hackage.haskell.org/package/base
import Control.Exception (ErrorCall, Exception, evaluate, handle)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
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

-- HLint does not support typed expression splices
{-# ANN module ("HLint: ignore" :: String) #-}

------------------------------------------------------------------------------
-- $HelperFunctions

assertRaises
  :: (Exception e, Show a)
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

xBS :: BS.ByteString
xBS = "test \xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88"

xBSL :: BSL.ByteString
xBSL = "test \xe3\x83\x86\xe3\x82\xb9\xe3\x83\x88"

xTLB :: TLB.Builder
xTLB = "test テスト"

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

xiBS :: BS.ByteString
xiBS = "test \xe3"

xiBSL :: BSL.ByteString
xiBSL = "test \xe3"

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

answerBS :: BS.ByteString
answerBS = "42"

answerBSL :: BSL.ByteString
answerBSL = "42"

answerZ :: Int
answerZ = 42

data IntError = IntInvalid
  deriving (Eq, Show)

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

redBS :: BS.ByteString
redBS = "red"

redBSL :: BSL.ByteString
redBSL = "red"

------------------------------------------------------------------------------
-- $Textual

testToS :: TestTree
testToS = testGroup "toS"
    [ testCase "S" $ xS @=? TTC.toS xS
    , testCase "T" $ xS @=? TTC.toS xT
    , testCase "TL" $ xS @=? TTC.toS xTL
    , testCase "BS" $ xS @=? TTC.toS xBS
    , testCase "BS/invalid" $ xiS @=? TTC.toS xiBS
    , testCase "BSL" $ xS @=? TTC.toS xBSL
    , testCase "BSL/invalid" $ xiS @=? TTC.toS xiBSL
    ]

testToT :: TestTree
testToT = testGroup "toT"
    [ testCase "S" $ xT @=? TTC.toT xS
    , testCase "T" $ xT @=? TTC.toT xT
    , testCase "TL" $ xT @=? TTC.toT xTL
    , testCase "BS" $ xT @=? TTC.toT xBS
    , testCase "BS/invalid" $ xiT @=? TTC.toT xiBS
    , testCase "BSL" $ xT @=? TTC.toT xBSL
    , testCase "BSL/invalid" $ xiT @=? TTC.toT xiBSL
    ]

testToTL :: TestTree
testToTL = testGroup "toTL"
    [ testCase "S" $ xTL @=? TTC.toTL xS
    , testCase "T" $ xTL @=? TTC.toTL xT
    , testCase "TL" $ xTL @=? TTC.toTL xTL
    , testCase "BS" $ xTL @=? TTC.toTL xBS
    , testCase "BS/invalid" $ xiTL @=? TTC.toTL xiBS
    , testCase "BSL" $ xTL @=? TTC.toTL xBSL
    , testCase "BSL/invalid" $ xiTL @=? TTC.toTL xiBSL
    ]

testToBS :: TestTree
testToBS = testGroup "toBS"
    [ testCase "S" $ xBS @=? TTC.toBS xS
    , testCase "T" $ xBS @=? TTC.toBS xT
    , testCase "TL" $ xBS @=? TTC.toBS xTL
    , testCase "BS" $ xBS @=? TTC.toBS xBS
    , testCase "BSL" $ xBS @=? TTC.toBS xBSL
    ]

testToBSL :: TestTree
testToBSL = testGroup "toBSL"
    [ testCase "S" $ xBSL @=? TTC.toBSL xS
    , testCase "T" $ xBSL @=? TTC.toBSL xT
    , testCase "TL" $ xBSL @=? TTC.toBSL xTL
    , testCase "BS" $ xBSL @=? TTC.toBSL xBS
    , testCase "BSL" $ xBSL @=? TTC.toBSL xBSL
    ]

testConvert :: TestTree
testConvert = testGroup "convert"
    [ testCase "S->S" $ xS @=? TTC.convert xS
    , testCase "S->T" $ xT @=? TTC.convert xS
    , testCase "S->TL" $ xTL @=? TTC.convert xS
    , testCase "S->BS" $ xBS @=? TTC.convert xS
    , testCase "S->BSL" $ xBSL @=? TTC.convert xS
    , testCase "T->S" $ xS @=? TTC.convert xT
    , testCase "T->T" $ xT @=? TTC.convert xT
    , testCase "T->TL" $ xTL @=? TTC.convert xT
    , testCase "T->BS" $ xBS @=? TTC.convert xT
    , testCase "T->BSL" $ xBSL @=? TTC.convert xT
    , testCase "TL->S" $ xS @=? TTC.convert xTL
    , testCase "TL->T" $ xT @=? TTC.convert xTL
    , testCase "TL->TL" $ xTL @=? TTC.convert xTL
    , testCase "TL->BS" $ xBS @=? TTC.convert xTL
    , testCase "TL->BSL" $ xBSL @=? TTC.convert xTL
    , testCase "BS->S" $ xS @=? TTC.convert xBS
    , testCase "BS->T" $ xT @=? TTC.convert xBS
    , testCase "BS->TL" $ xTL @=? TTC.convert xBS
    , testCase "BS->BS" $ xBS @=? TTC.convert xBS
    , testCase "BS->BSL" $ xBSL @=? TTC.convert xBS
    , testCase "BSL->S" $ xS @=? TTC.convert xBSL
    , testCase "BSL->T" $ xT @=? TTC.convert xBSL
    , testCase "BSL->TL" $ xTL @=? TTC.convert xBSL
    , testCase "BSL->BS" $ xBS @=? TTC.convert xBSL
    , testCase "BSL->BSL" $ xBSL @=? TTC.convert xBSL
    ]

testFromS :: TestTree
testFromS = testGroup "fromS"
    [ testCase "S" $ xS @=? TTC.fromS xS
    , testCase "T" $ xT @=? TTC.fromS xS
    , testCase "TL" $ xTL @=? TTC.fromS xS
    , testCase "BS" $ xBS @=? TTC.fromS xS
    , testCase "BSL" $ xBSL @=? TTC.fromS xS
    ]

testFromT :: TestTree
testFromT = testGroup "fromT"
    [ testCase "S" $ xS @=? TTC.fromT xT
    , testCase "T" $ xT @=? TTC.fromT xT
    , testCase "TL" $ xTL @=? TTC.fromT xT
    , testCase "BS" $ xBS @=? TTC.fromT xT
    , testCase "BSL" $ xBSL @=? TTC.fromT xT
    ]

testFromTL :: TestTree
testFromTL = testGroup "fromTL"
    [ testCase "S" $ xS @=? TTC.fromTL xTL
    , testCase "T" $ xT @=? TTC.fromTL xTL
    , testCase "TL" $ xTL @=? TTC.fromTL xTL
    , testCase "BS" $ xBS @=? TTC.fromTL xTL
    , testCase "BSL" $ xBSL @=? TTC.fromTL xTL
    ]

testFromBS :: TestTree
testFromBS = testGroup "fromBS"
    [ testCase "S" $ xS @=? TTC.fromBS xBS
    , testCase "T" $ xT @=? TTC.fromBS xBS
    , testCase "TL" $ xTL @=? TTC.fromBS xBS
    , testCase "BS" $ xBS @=? TTC.fromBS xBS
    , testCase "BSL" $ xBSL @=? TTC.fromBS xBS
    ]

testFromBSL :: TestTree
testFromBSL = testGroup "fromBSL"
    [ testCase "S" $ xS @=? TTC.fromBSL xBSL
    , testCase "T" $ xT @=? TTC.fromBSL xBSL
    , testCase "TL" $ xTL @=? TTC.fromBSL xBSL
    , testCase "BS" $ xBS @=? TTC.fromBSL xBSL
    , testCase "BSL" $ xBSL @=? TTC.fromBSL xBSL
    ]

testAsS :: TestTree
testAsS = testGroup "asS"
    [ testCase "S" $ xS @=? TTC.asS id xS
    , testCase "T" $ xS @=? TTC.asS id xT
    , testCase "TL" $ xS @=? TTC.asS id xTL
    , testCase "BS" $ xS @=? TTC.asS id xBS
    , testCase "BSL" $ xS @=? TTC.asS id xBSL
    ]

testAsT :: TestTree
testAsT = testGroup "asT"
    [ testCase "S" $ xT @=? TTC.asT id xS
    , testCase "T" $ xT @=? TTC.asT id xT
    , testCase "TL" $ xT @=? TTC.asT id xTL
    , testCase "BS" $ xT @=? TTC.asT id xBS
    , testCase "BSL" $ xT @=? TTC.asT id xBSL
    ]

testAsTL :: TestTree
testAsTL = testGroup "asTL"
    [ testCase "S" $ xTL @=? TTC.asTL id xS
    , testCase "T" $ xTL @=? TTC.asTL id xT
    , testCase "TL" $ xTL @=? TTC.asTL id xTL
    , testCase "BS" $ xTL @=? TTC.asTL id xBS
    , testCase "BSL" $ xTL @=? TTC.asTL id xBSL
    ]

testAsBS :: TestTree
testAsBS = testGroup "asBS"
    [ testCase "S" $ xBS @=? TTC.asBS id xS
    , testCase "T" $ xBS @=? TTC.asBS id xT
    , testCase "TL" $ xBS @=? TTC.asBS id xTL
    , testCase "BS" $ xBS @=? TTC.asBS id xBS
    , testCase "BSL" $ xBS @=? TTC.asBS id xBSL
    ]

testAsBSL :: TestTree
testAsBSL = testGroup "asBSL"
    [ testCase "S" $ xBSL @=? TTC.asBSL id xS
    , testCase "T" $ xBSL @=? TTC.asBSL id xT
    , testCase "TL" $ xBSL @=? TTC.asBSL id xTL
    , testCase "BS" $ xBSL @=? TTC.asBSL id xBS
    , testCase "BSL" $ xBSL @=? TTC.asBSL id xBSL
    ]

testToTLB :: TestTree
testToTLB = testCase "toTLB" $ xTLB @=? TTC.toTLB xS

testFromTLB :: TestTree
testFromTLB = testCase "fromTLB" $ xS @=? TTC.fromTLB xTLB

testToBSB :: TestTree
testToBSB = testCase "toBSB" $
    BSB.toLazyByteString xBSB @=? BSB.toLazyByteString (TTC.toBSB xS)

testFromBSB :: TestTree
testFromBSB = testCase "fromBSB" $ xS @=? TTC.fromBSB xBSB

testToSBS :: TestTree
testToSBS = testCase "toSBS" $ xSBS @=? TTC.toSBS xS

testFromSBS :: TestTree
testFromSBS = testCase "fromSBS" $ xS @=? TTC.fromSBS xSBS

------------------------------------------------------------------------------
-- $Render

testRender :: TestTree
testRender = testGroup "render"
    [ testCase "S" $ answerS @=? TTC.render answer
    , testCase "T" $ answerT @=? TTC.render answer
    , testCase "TL" $ answerTL @=? TTC.render answer
    , testCase "BS" $ answerBS @=? TTC.render answer
    , testCase "BSL" $ answerBSL @=? TTC.render answer
    ]

testRenderS :: TestTree
testRenderS = testCase "renderS" $ answerS @=? TTC.renderS answer

testRenderT :: TestTree
testRenderT = testCase "renderT" $ answerT @=? TTC.renderT answer

testRenderTL :: TestTree
testRenderTL = testCase "renderTL" $ answerTL @=? TTC.renderTL answer

testRenderBS :: TestTree
testRenderBS = testCase "renderBS" $ answerBS @=? TTC.renderBS answer

testRenderBSL :: TestTree
testRenderBSL = testCase "renderBSL" $ answerBSL @=? TTC.renderBSL answer

testRenderWithShow :: TestTree
testRenderWithShow = testGroup "renderWithShow"
    [ testCase "S" $ answerS @=? TTC.renderWithShow answerZ
    , testCase "T" $ answerT @=? TTC.renderWithShow answerZ
    , testCase "TL" $ answerTL @=? TTC.renderWithShow answerZ
    , testCase "BS" $ answerBS @=? TTC.renderWithShow answerZ
    , testCase "BSL" $ answerBSL @=? TTC.renderWithShow answerZ
    ]

------------------------------------------------------------------------------
-- $Parse

testParse :: TestTree
testParse = testGroup "parse"
    [ testCase "S" $ Just answer @=? TTC.parseMaybe answerS
    , testCase "T" $ Just answer @=? TTC.parseMaybe answerT
    , testCase "TL" $ Just answer @=? TTC.parseMaybe answerTL
    , testCase "BS" $ Just answer @=? TTC.parseMaybe answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseMaybe answerBSL
    , testCase "negative" $ Left "not positive" @=?
        (TTC.parse ('-' : answerS) :: Either String PosInt)
    , testCase "invalid" $ Left "not an integer" @=?
        (TTC.parse ('a' : answerS) :: Either String PosInt)
    ]

testParseS :: TestTree
testParseS = testCase "parseS" $ Just answer @=? TTC.parseMaybeS answerS

testParseT :: TestTree
testParseT = testCase "parseT" $ Just answer @=? TTC.parseMaybeT answerT

testParseTL :: TestTree
testParseTL = testCase "parseTL" $ Just answer @=? TTC.parseMaybeTL answerTL

testParseBS :: TestTree
testParseBS = testCase "parseBS" $ Just answer @=? TTC.parseMaybeBS answerBS

testParseBSL :: TestTree
testParseBSL =
    testCase "parseBSL" $ Just answer @=? TTC.parseMaybeBSL answerBSL

testParseMaybe :: TestTree
testParseMaybe = testGroup "parseMaybe"
    [ testCase "S" $ Just answer @=? TTC.parseMaybe answerS
    , testCase "T" $ Just answer @=? TTC.parseMaybe answerT
    , testCase "TL" $ Just answer @=? TTC.parseMaybe answerTL
    , testCase "BS" $ Just answer @=? TTC.parseMaybe answerBS
    , testCase "BSL" $ Just answer @=? TTC.parseMaybe answerBSL
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

testParseMaybeBS :: TestTree
testParseMaybeBS = testCase "parseMaybeBS" $
    Just answer @=? TTC.parseMaybeBS answerBS

testParseMaybeBSL :: TestTree
testParseMaybeBSL = testCase "parseMaybeBSL" $
    Just answer @=? TTC.parseMaybeBSL answerBSL

testParseUnsafe :: TestTree
testParseUnsafe = testGroup "parseUnsafe"
    [ testCase "S" $ answer @=? TTC.parseUnsafe answerS
    , testCase "T" $ answer @=? TTC.parseUnsafe answerT
    , testCase "TL" $ answer @=? TTC.parseUnsafe answerTL
    , testCase "BS" $ answer @=? TTC.parseUnsafe answerBS
    , testCase "BSL" $ answer @=? TTC.parseUnsafe answerBSL
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

testParseUnsafeBS :: TestTree
testParseUnsafeBS = testCase "parseUnsafeBS" $
    answer @=? TTC.parseUnsafeBS answerBS

testParseUnsafeBSL :: TestTree
testParseUnsafeBSL = testCase "parseUnsafeBSL" $
    answer @=? TTC.parseUnsafeBSL answerBSL

testParseWithRead :: TestTree
testParseWithRead = testGroup "parseWithRead"
    [ testCase "S" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerS
    , testCase "T" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerT
    , testCase "TL" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerTL
    , testCase "BS" $ Right answerZ @=? TTC.parseWithRead IntInvalid answerBS
    , testCase "BSL" $
        Right answerZ @=? TTC.parseWithRead IntInvalid answerBSL
    , testCase "invalid" $ Left IntInvalid @=?
        (TTC.parseWithRead IntInvalid ('a' : answerS) :: Either IntError Int)
    ]

testParseWithRead' :: TestTree
testParseWithRead' = testGroup "parseWithRead'"
    [ testCase "S" $ Right answerZ @=?
        (TTC.parseWithRead' "Int" answerS :: Either String Int)
    , testCase "T" $ Right answerZ @=?
        (TTC.parseWithRead' "Int" answerT :: Either String Int)
    , testCase "TL" $ Right answerZ @=?
        (TTC.parseWithRead' "Int" answerTL :: Either String Int)
    , testCase "BS" $ Right answerZ @=?
        (TTC.parseWithRead' "Int" answerBS :: Either String Int)
    , testCase "BSL" $ Right answerZ @=?
        (TTC.parseWithRead' "Int" answerBSL :: Either String Int)
    , testCase "invalid" $ Left "invalid Int" @=?
        (TTC.parseWithRead' "Int" ('a' : answerS) :: Either String Int)
    ]

testMaybeParseWithRead :: TestTree
testMaybeParseWithRead = testGroup "maybeParseWithRead"
    [ testCase "S" $ Just answerZ @=? TTC.maybeParseWithRead answerS
    , testCase "T" $ Just answerZ @=? TTC.maybeParseWithRead answerT
    , testCase "TL" $ Just answerZ @=? TTC.maybeParseWithRead answerTL
    , testCase "BS" $ Just answerZ @=? TTC.maybeParseWithRead answerBS
    , testCase "BSL" $ Just answerZ @=? TTC.maybeParseWithRead answerBSL
    , testCase "invalid" $ Nothing @=?
        (TTC.maybeParseWithRead ('a' : answerS) :: Maybe Int)
    ]

testParseEnum :: TestTree
testParseEnum = testGroup "parseEnum"
    [ testCase "S" $ Right Red @=? parse False False redS
    , testCase "T" $ Right Red @=? parse False False redT
    , testCase "TL" $ Right Red @=? parse False False redTL
    , testCase "BS" $ Right Red @=? parse False False redBS
    , testCase "BSL" $ Right Red @=? parse False False redBSL
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
    , testCase "BS" $ Right Red @=? parse False False redBS
    , testCase "BSL" $ Right Red @=? parse False False redBSL
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
        , testToBS
        , testToBSL
        , testConvert
        , testFromS
        , testFromT
        , testFromTL
        , testFromBS
        , testFromBSL
        , testAsS
        , testAsT
        , testAsTL
        , testAsBS
        , testAsBSL
        , testToTLB
        , testFromTLB
        , testToBSB
        , testFromBSB
        , testToSBS
        , testFromSBS
        ]
    , testGroup "Render"
        [ testRender
        , testRenderS
        , testRenderT
        , testRenderTL
        , testRenderBS
        , testRenderBSL
        , testRenderWithShow
        ]
    , testGroup "Parse"
        [ testParse
        , testParseS
        , testParseT
        , testParseTL
        , testParseBS
        , testParseBSL
        , testParseMaybe
        , testParseMaybeS
        , testParseMaybeT
        , testParseMaybeTL
        , testParseMaybeBS
        , testParseMaybeBSL
        , testParseUnsafe
        , testParseUnsafeS
        , testParseUnsafeT
        , testParseUnsafeTL
        , testParseUnsafeBS
        , testParseUnsafeBSL
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
