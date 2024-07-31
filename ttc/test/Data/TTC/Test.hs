{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.TTC.Test (tests) where

-- https://hackage.haskell.org/package/base
import Control.Exception (ErrorCall, Exception, evaluate, handle)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.String (IsString)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Stack (HasCallStack)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestName, TestTree, testGroup)

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
import qualified TestTypes.Ex as Ex
import TestTypes.Ex (Ex(Ex))

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

-- U+FFFD is the Unicode replacement character
xiS :: String
xiS = "test \xfffd"

xiT :: T.Text
xiT = T.pack xiS

xiTL :: TL.Text
xiTL = TL.pack xiS

xiTLB :: TLB.Builder
xiTLB = TLB.fromString xiS

xiST :: ST.ShortText
xiST = ST.pack xiS

-- Lone continuation byte is invalid
xiBS :: BS.ByteString
xiBS = "test \xe3"

xiBSL :: BSL.ByteString
xiBSL = BSL.fromStrict xiBS

xiBSB :: BSB.Builder
xiBSB = BSB.byteString xiBS

xiSBS :: SBS.ShortByteString
xiSBS = SBS.toShort xiBS

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
testConvert = testGroup "convert" $
    [ testCase "@@" $
        "test テスト" @=? TTC.convert @String @String "test テスト"
    , testCase "@_" $ xT @=? TTC.convert @String "test テスト"
    , testCase "_@" $ "test テスト" @=? TTC.convert @_ @TL.Text xT
    ]
    ++ mkTests "S"   xS
    ++ mkTests "T"   xT
    ++ mkTests "TL"  xTL
    ++ mkTests "TLB" xTLB
    ++ mkTests "ST"  xST
    ++ mkTests "BS"  xBS
    ++ mkTests "BSL" xBSL
    ++ mkTests "BSB" xBSB
    ++ mkTests "SBS" xSBS
  where
    mkTests :: TTC.Textual a => String -> a -> [TestTree]
    mkTests s x =
      [ testCase (s ++ "->S")   $ xS   @=? TTC.convert x
      , testCase (s ++ "->T")   $ xT   @=? TTC.convert x
      , testCase (s ++ "->TL")  $ xTL  @=? TTC.convert x
      , testCase (s ++ "->TLB") $ xTLB @=? TTC.convert x
      , testCase (s ++ "->ST")  $ xST  @=? TTC.convert x
      , testCase (s ++ "->BS")  $ xBS  @=? TTC.convert x
      , testCase (s ++ "->BSL") $ xBSL @=? TTC.convert x
      , testCase (s ++ "->BSB") $ xBSB @=? TTC.convert x
      , testCase (s ++ "->SBS") $ xSBS @=? TTC.convert x
      ]

------------------------------------------------------------------------------

testToX :: TestTree
testToX = testGroup "to*"
    [ mkTests "toS"   TTC.toS   xS   xiS
    , mkTests "toT"   TTC.toT   xT   xiT
    , mkTests "toTL"  TTC.toTL  xTL  xiTL
    , mkTests "toTLB" TTC.toTLB xTLB xiTLB
    , mkTests "toST"  TTC.toST  xST  xiST
    , mkTests "toBS"  TTC.toBS  xBS  xiBS
    , mkTests "toBSL" TTC.toBSL xBSL xiBSL
    , mkTests "toBSB" TTC.toBSB xBSB xiBSB
    , mkTests "toSBS" TTC.toSBS xSBS xiSBS
    ]
  where
    mkTests
      :: (Eq a, Show a)
      => TestName
      -> (forall t. TTC.Textual t => t -> a)
      -> a
      -> a
      -> TestTree
    mkTests testName f x xi = testGroup testName
      [ testCase "@"           $ x  @=? f @T.Text "test テスト"
      , testCase "S"           $ x  @=? f         xS
      , testCase "T"           $ x  @=? f         xT
      , testCase "TL"          $ x  @=? f         xTL
      , testCase "TLB"         $ x  @=? f         xTLB
      , testCase "ST"          $ x  @=? f         xST
      , testCase "BS"          $ x  @=? f         xBS
      , testCase "BSL"         $ x  @=? f         xBSL
      , testCase "BSB"         $ x  @=? f         xBSB
      , testCase "SBS"         $ x  @=? f         xSBS
      , testCase "BS/invalid"  $ xi @=? f         xiBS
      , testCase "BSL/invalid" $ xi @=? f         xiBSL
      , testCase "BSB/invalid" $ xi @=? f         xiBSB
      , testCase "SBS/invalid" $ xi @=? f         xiSBS
      ]

------------------------------------------------------------------------------

testFromX :: TestTree
testFromX = testGroup "from*"
    [ mkTests "fromS"   TTC.fromS   xS   Nothing
    , mkTests "fromT"   TTC.fromT   xT   Nothing
    , mkTests "fromTL"  TTC.fromTL  xTL  Nothing
    , mkTests "fromTLB" TTC.fromTLB xTLB Nothing
    , mkTests "fromST"  TTC.fromST  xST  Nothing
    , mkTests "fromBS"  TTC.fromBS  xBS  (Just xiBS)
    , mkTests "fromBSL" TTC.fromBSL xBSL (Just xiBSL)
    , mkTests "fromBSB" TTC.fromBSB xBSB (Just xiBSB)
    , mkTests "fromSBS" TTC.fromSBS xSBS (Just xiSBS)
    ]
  where
    mkTests
      :: (Eq a, Show a)
      => TestName
      -> (forall t. TTC.Textual t => a -> t)
      -> a
      -> Maybe a
      -> TestTree
    mkTests testName f x mXi = testGroup testName $
      [ testCase "@"   $ "test テスト" @=? f @String x
      , testCase "S"   $ xS            @=? f         x
      , testCase "T"   $ xT            @=? f         x
      , testCase "TL"  $ xTL           @=? f         x
      , testCase "TLB" $ xTLB          @=? f         x
      , testCase "ST"  $ xST           @=? f         x
      , testCase "BS"  $ xBS           @=? f         x
      , testCase "BSL" $ xBSL          @=? f         x
      , testCase "BSB" $ xBSB          @=? f         x
      , testCase "SBS" $ xSBS          @=? f         x
      ] ++
      case mXi of
        Nothing -> []
        Just xi ->
          [ testCase "S/Invalid"   $ xiS   @=? f xi
          , testCase "T/Invalid"   $ xiT   @=? f xi
          , testCase "TL/Invalid"  $ xiTL  @=? f xi
          , testCase "TLB/Invalid" $ xiTLB @=? f xi
          , testCase "ST/Invalid"  $ xiST  @=? f xi
          , testCase "BS/Invalid"  $ xiBS  @=? f xi
          , testCase "BSL/Invalid" $ xiBSL @=? f xi
          , testCase "BSB/Invalid" $ xiBSB @=? f xi
          , testCase "SBS/Invalid" $ xiSBS @=? f xi
          ]

------------------------------------------------------------------------------

testAsX :: TestTree
testAsX = testGroup "as*"
    [ mkTests "asS"   TTC.asS   xS   xiS
    , mkTests "asT"   TTC.asT   xT   xiT
    , mkTests "asTL"  TTC.asTL  xTL  xiTL
    , mkTests "asTLB" TTC.asTLB xTLB xiTLB
    , mkTests "asST"  TTC.asST  xST  xiST
    , mkTests "asBS"  TTC.asBS  xBS  xiBS
    , mkTests "asBSL" TTC.asBSL xBSL xiBSL
    , mkTests "asBSB" TTC.asBSB xBSB xiBSB
    , mkTests "asSBS" TTC.asSBS xSBS xiSBS
    ]
  where
    mkTests
      :: (Eq a, Show a)
      => TestName
      -> (forall t. TTC.Textual t => (a -> a) -> t -> a)
      -> a
      -> a
      -> TestTree
    mkTests testName f x xi = testGroup testName
      [ testCase "@"           $ x  @=? f @T.Text id "test テスト"
      , testCase "S"           $ x  @=? f         id xS
      , testCase "T"           $ x  @=? f         id xT
      , testCase "TL"          $ x  @=? f         id xTL
      , testCase "TLB"         $ x  @=? f         id xTLB
      , testCase "ST"          $ x  @=? f         id xST
      , testCase "BS"          $ x  @=? f         id xBS
      , testCase "BSL"         $ x  @=? f         id xBSL
      , testCase "BSB"         $ x  @=? f         id xBSB
      , testCase "SBS"         $ x  @=? f         id xSBS
      , testCase "BS/invalid"  $ xi @=? f         id xiBS
      , testCase "BSL/invalid" $ xi @=? f         id xiBSL
      , testCase "BSB/invalid" $ xi @=? f         id xiBSB
      , testCase "SBS/invalid" $ xi @=? f         id xiSBS
      ]

------------------------------------------------------------------------------
-- $Render

testRender :: TestTree
testRender = testGroup "render"
    [ testCase "_@"  $ "42"      @=? TTC.render @_ @String answer
    , testCase "S"   $ answerS   @=? TTC.render            answer
    , testCase "T"   $ answerT   @=? TTC.render            answer
    , testCase "TL"  $ answerTL  @=? TTC.render            answer
    , testCase "TLB" $ answerTLB @=? TTC.render            answer
    , testCase "ST"  $ answerST  @=? TTC.render            answer
    , testCase "BS"  $ answerBS  @=? TTC.render            answer
    , testCase "BSL" $ answerBSL @=? TTC.render            answer
    , testCase "BSB" $ answerBSB @=? TTC.render            answer
    , testCase "SBS" $ answerSBS @=? TTC.render            answer
    ]

------------------------------------------------------------------------------

testRenderDefault :: TestTree
testRenderDefault = testGroup "RenderDefault"
    [ testCase "Bool" $ "True" @=? TTC.renderS True
    , testCase "Char" $ "*"    @=? TTC.renderS '*'
    , mkTestShow @Double  "Double"  3.14159
    , mkTestShow @Float   "Float"   3.14159
    , mkTestShow @Integer "Integer" 42
    , mkTestShow @Int     "Int"     42
    , mkTestShow @Int8    "Int8"    42
    , mkTestShow @Int16   "Int16"   42
    , mkTestShow @Int32   "Int32"   42
    , mkTestShow @Int64   "Int64"   42
    , mkTestShow @Word    "Word"    42
    , mkTestShow @Word8   "Word8"   42
    , mkTestShow @Word16  "Word16"  42
    , mkTestShow @Word32  "Word32"  42
    , mkTestShow @Word64  "Word64"  42
    , testCase "String"              $ xS @=? TTC.renderS xS
    , testCase "T.Text"              $ xS @=? TTC.renderS xT
    , testCase "TL.Text"             $ xS @=? TTC.renderS xTL
    , testCase "TLB.Builder"         $ xS @=? TTC.renderS xTLB
    , testCase "ST.ShortText"        $ xS @=? TTC.renderS xST
    , testCase "BS.ByteString"       $ xS @=? TTC.renderS xBS
    , testCase "BSL.ByteString"      $ xS @=? TTC.renderS xBSL
    , testCase "BSB.Builder"         $ xS @=? TTC.renderS xBSB
    , testCase "SBS.ShortByteString" $ xS @=? TTC.renderS xSBS
    ]
  where
    mkTestShow :: (Show a, TTC.Render a) => TestName -> a -> TestTree
    mkTestShow testName x = testCase testName $ show x @=? TTC.renderS x

------------------------------------------------------------------------------

testRenderWithShow :: TestTree
testRenderWithShow = testGroup "renderWithShow"
    [ testCase "@"   $ "42"      @=? TTC.renderWithShow @String answerZ
    , testCase "S"   $ answerS   @=? TTC.renderWithShow         answerZ
    , testCase "T"   $ answerT   @=? TTC.renderWithShow         answerZ
    , testCase "TL"  $ answerTL  @=? TTC.renderWithShow         answerZ
    , testCase "TLB" $ answerTLB @=? TTC.renderWithShow         answerZ
    , testCase "ST"  $ answerST  @=? TTC.renderWithShow         answerZ
    , testCase "BS"  $ answerBS  @=? TTC.renderWithShow         answerZ
    , testCase "BSL" $ answerBSL @=? TTC.renderWithShow         answerZ
    , testCase "BSB" $ answerBSB @=? TTC.renderWithShow         answerZ
    , testCase "SBS" $ answerSBS @=? TTC.renderWithShow         answerZ
    ]

------------------------------------------------------------------------------

testRenderX :: TestTree
testRenderX = testGroup "render*"
    [ testCase "renderS"   $ answerS   @=? TTC.renderS   answer
    , testCase "renderT"   $ answerT   @=? TTC.renderT   answer
    , testCase "renderTL"  $ answerTL  @=? TTC.renderTL  answer
    , testCase "renderTLB" $ answerTLB @=? TTC.renderTLB answer
    , testCase "renderST"  $ answerST  @=? TTC.renderST  answer
    , testCase "renderBS"  $ answerBS  @=? TTC.renderBS  answer
    , testCase "renderBSL" $ answerBSL @=? TTC.renderBSL answer
    , testCase "renderBSB" $ answerBSB @=? TTC.renderBSB answer
    , testCase "renderSBS" $ answerSBS @=? TTC.renderSBS answer
    ]

------------------------------------------------------------------------------
-- $Parse

testParse :: TestTree
testParse = testGroup "parse"
    [ testCase "_@@" $ Right answer @=? TTC.parse @_ @String @String "42"
    , testCase "S"   $ Right answer @=? parse answerS
    , testCase "T"   $ Right answer @=? parse answerT
    , testCase "TL"  $ Right answer @=? parse answerTL
    , testCase "TLB" $ Right answer @=? parse answerTLB
    , testCase "ST"  $ Right answer @=? parse answerST
    , testCase "BS"  $ Right answer @=? parse answerBS
    , testCase "BSL" $ Right answer @=? parse answerBSL
    , testCase "BSB" $ Right answer @=? parse answerBSB
    , testCase "SBS" $ Right answer @=? parse answerSBS
    , testCase "negative" $ Left "not positive"   @=? parse @String "-42"
    , testCase "invalid"  $ Left "not an integer" @=? parse @String "4a2"
    ]
  where
    parse :: TTC.Textual t => t -> Either String PosInt
    parse = TTC.parse

------------------------------------------------------------------------------

testParseDefault :: TestTree
testParseDefault = testGroup "ParseDefault"
    [ testGroup "Bool"
        [ testCase "True"    $ Right True          @=? parse       "True"
        , testCase "False"   $ Right False         @=? parse       "False"
        , testCase "invalid" $ Left "invalid Bool" @=? parse @Bool "false"
        ]
    , testGroup "Char"
        [ testCase "valid"    $ Right '*'           @=? parse       "*"
        , testCase "empty"    $ Left "invalid Char" @=? parse @Char ""
        , testCase "multiple" $ Left "invalid Char" @=? parse @Char "**"
        ]
    , mkTestsShow @Double  "Double"  3.14159
    , mkTestsShow @Float   "Float"   3.14159
    , mkTestsShow @Integer "Integer" 42
    , mkTestsShow @Int     "Int"     42
    , mkTestsShow @Int8    "Int8"    42
    , mkTestsShow @Int16   "Int16"   42
    , mkTestsShow @Int32   "Int32"   42
    , mkTestsShow @Int64   "Int64"   42
    , mkTestsShow @Word    "Word"    42
    , mkTestsShow @Word8   "Word8"   42
    , mkTestsShow @Word16  "Word16"  42
    , mkTestsShow @Word32  "Word32"  42
    , mkTestsShow @Word64  "Word64"  42
    , mkTestsTextual @String              "String"              xS
    , mkTestsTextual @T.Text              "T.Text"              xT
    , mkTestsTextual @TL.Text             "TL.Text"             xTL
    , mkTestsTextual @TLB.Builder         "TLB.Builder"         xTLB
    , mkTestsTextual @ST.ShortText        "ST.ShortText"        xST
    , mkTestsTextual @BS.ByteString       "BS.ByteString"       xBS
    , mkTestsTextual @BSL.ByteString      "BSL.ByteString"      xBSL
    , mkTestsTextual @BSB.Builder         "BSB.Builder"         xBSB
    , mkTestsTextual @SBS.ShortByteString "SBS.ShortByteString" xSBS
    ]
  where
    mkTestsShow
      :: forall a. (Eq a, Show a, TTC.Parse a)
      => TestName
      -> a
      -> TestTree
    mkTestsShow testName x = testGroup testName
      [ testCase "valid" $ Right x @=? parse (show x)
      , testCase "invalid" $
          Left ("invalid " ++ testName) @=? parse @a "invalid"
      ]

    mkTestsTextual
      :: forall a. (Eq a, IsString a, Show a, TTC.Parse a)
      => TestName
      -> a
      -> TestTree
    mkTestsTextual testName x = testGroup testName
      [ testCase "empty"    $ Right "" @=? parse @a ""
      , testCase "nonempty" $ Right x  @=? parse xS
      ]

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

testWithErrorX :: TestTree
testWithErrorX = testGroup "withError*"
    [ mkTests "withErrorS"   TTC.withErrorS
    , mkTests "withErrorT"   TTC.withErrorT
    , mkTests "withErrorTL"  TTC.withErrorTL
    , mkTests "withErrorTLB" TTC.withErrorTLB
    , mkTests "withErrorST"  TTC.withErrorST
    , mkTests "withErrorBS"  TTC.withErrorBS
    , mkTests "withErrorBSL" TTC.withErrorBSL
    , mkTests "withErrorBSB" TTC.withErrorBSB
    , mkTests "withErrorSBS" TTC.withErrorSBS
    ]
  where
    mkTests
      :: IsString e'
      => TestName
      -> (e' -> Maybe PosInt -> Either String PosInt)
      -> TestTree
    mkTests testName f = testGroup testName
      [ testCase "valid"   $ Right answer @=? f undefined (Just answer)
      , testCase "invalid" $ Left "err"   @=? f "err"     Nothing
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

testPrefixErrorX :: TestTree
testPrefixErrorX = testGroup "prefixError*"
    [ mkTests "prefixErrorS"   TTC.prefixErrorS
    , mkTests "prefixErrorT"   TTC.prefixErrorT
    , mkTests "prefixErrorTL"  TTC.prefixErrorTL
    , mkTests "prefixErrorTLB" TTC.prefixErrorTLB
    , mkTests "prefixErrorST"  TTC.prefixErrorST
    , mkTests "prefixErrorBS"  TTC.prefixErrorBS
    , mkTests "prefixErrorBSL" TTC.prefixErrorBSL
    , mkTests "prefixErrorBSB" TTC.prefixErrorBSB
    , mkTests "prefixErrorSBS" TTC.prefixErrorSBS
    ]
  where
    mkTests
      :: IsString e'
      => TestName
      -> (e' -> Either e' PosInt -> Either String PosInt)
      -> TestTree
    mkTests testName f = testGroup testName
      [ testCase "valid"   $ Right answer     @=? f "oops: " (Right answer)
      , testCase "invalid" $ Left "oops: err" @=? f "oops: " (Left "err")
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

testParseX :: TestTree
testParseX = testGroup "parse*"
    [ mkTests "parseS"   TTC.parseS   answerS
    , mkTests "parseT"   TTC.parseT   answerT
    , mkTests "parseTL"  TTC.parseTL  answerTL
    , mkTests "parseTLB" TTC.parseTLB answerTLB
    , mkTests "parseST"  TTC.parseST  answerST
    , mkTests "parseBS"  TTC.parseBS  answerBS
    , mkTests "parseBSL" TTC.parseBSL answerBSL
    , mkTests "parseBSB" TTC.parseBSB answerBSB
    , mkTests "parseSBS" TTC.parseSBS answerSBS
    ]
  where
    mkTests
      :: IsString a
      => TestName
      -> (a -> Either String PosInt)
      -> a
      -> TestTree
    mkTests testName f x = testGroup testName
      [ testCase "valid"   $ Right answer          @=? f x
      , testCase "invalid" $ Left "not an integer" @=? f "4a2"
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

testParseMaybeX :: TestTree
testParseMaybeX = testGroup "parseMaybe*"
    [ mkTestsParseMaybe "parseMaybeS"   TTC.parseMaybeS   answerS
    , mkTestsParseMaybe "parseMaybeT"   TTC.parseMaybeT   answerT
    , mkTestsParseMaybe "parseMaybeTL"  TTC.parseMaybeTL  answerTL
    , mkTestsParseMaybe "parseMaybeTLB" TTC.parseMaybeTLB answerTLB
    , mkTestsParseMaybe "parseMaybeST"  TTC.parseMaybeST  answerST
    , mkTestsParseMaybe "parseMaybeBS"  TTC.parseMaybeBS  answerBS
    , mkTestsParseMaybe "parseMaybeBSL" TTC.parseMaybeBSL answerBSL
    , mkTestsParseMaybe "parseMaybeBSB" TTC.parseMaybeBSB answerBSB
    , mkTestsParseMaybe "parseMaybeSBS" TTC.parseMaybeSBS answerSBS
    ]

mkTestsParseMaybe
  :: IsString a
  => TestName
  -> (a -> Maybe PosInt)
  -> a
  -> TestTree
mkTestsParseMaybe testName f x = testGroup testName
    [ testCase "valid"   $ Just answer @=? f x
    , testCase "invalid" $ Nothing     @=? f "4a2"
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

testParseOrFailX :: TestTree
testParseOrFailX = testGroup "parseOrFail*"
    [ mkTestsParseMaybe "parseOrFailS"   TTC.parseOrFailS   answerS
    , mkTestsParseMaybe "parseOrFailT"   TTC.parseOrFailT   answerT
    , mkTestsParseMaybe "parseOrFailTL"  TTC.parseOrFailTL  answerTL
    , mkTestsParseMaybe "parseOrFailTLB" TTC.parseOrFailTLB answerTLB
    , mkTestsParseMaybe "parseOrFailST"  TTC.parseOrFailST  answerST
    , mkTestsParseMaybe "parseOrFailBS"  TTC.parseOrFailBS  answerBS
    , mkTestsParseMaybe "parseOrFailBSL" TTC.parseOrFailBSL answerBSL
    , mkTestsParseMaybe "parseOrFailBSB" TTC.parseOrFailBSB answerBSB
    , mkTestsParseMaybe "parseOrFailSBS" TTC.parseOrFailSBS answerSBS
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

testParseUnsafeX :: TestTree
testParseUnsafeX = testGroup "parseUnsafe*"
    [ mkTests "parseUnsafeS"   TTC.parseUnsafeS   answerS
    , mkTests "parseUnsafeT"   TTC.parseUnsafeT   answerT
    , mkTests "parseUnsafeTL"  TTC.parseUnsafeTL  answerTL
    , mkTests "parseUnsafeTLB" TTC.parseUnsafeTLB answerTLB
    , mkTests "parseUnsafeST"  TTC.parseUnsafeST  answerST
    , mkTests "parseUnsafeBS"  TTC.parseUnsafeBS  answerBS
    , mkTests "parseUnsafeBSL" TTC.parseUnsafeBSL answerBSL
    , mkTests "parseUnsafeBSB" TTC.parseUnsafeBSB answerBSB
    , mkTests "parseUnsafeSBS" TTC.parseUnsafeSBS answerSBS
    ]
  where
    mkTests
      :: IsString a
      => TestName
      -> (a -> PosInt)
      -> a
      -> TestTree
    mkTests testName f x = testGroup testName
      [ testCase "valid" $ answer @=? f x
      , testCase "invalid" . assertRaises (Proxy :: Proxy ErrorCall) $ f "4a2"
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
testValid = testCase "valid" $ Ex "test" @=? validConst
  where
    validConst :: Ex
    validConst = $$(TTC.valid "test")

testValidIsString :: TestTree
testValidIsString =
    testCase "validIsString" $ Ex "test" @=? validConst
  where
    validConst :: Ex
    validConst = $$("test")

testValidOf :: TestTree
testValidOf = testCase "validOf" $
    Ex "test" @=? $$(TTC.validOf (Proxy :: Proxy Ex) "test")

testMkValid :: TestTree
testMkValid = testCase "mkValid" $ Ex "test" @=? $$(Ex.valid "test")

testUntypedValidOf :: TestTree
testUntypedValidOf = testCase "untypedValidOf" $
    Ex "test" @=? $(TTC.untypedValidOf (Proxy :: Proxy Ex) "test")

testMkUntypedValid :: TestTree
testMkUntypedValid = testCase "mkUntypedValid" $
    Ex "test" @=? $(Ex.untypedValid "test")

testMkUntypedValidQQ :: TestTree
testMkUntypedValidQQ = testCase "mkUntypedValidQQ" $
    Ex "test" @=? [Ex.untypedValidQQ|test|]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Data.TTC"
    [ testGroup "Textual"
        [ testConvert
        , testToX
        , testFromX
        , testAsX
        ]
    , testGroup "Render"
        [ testRender
        , testRenderDefault
        , testRenderWithShow
        , testRenderX
        ]
    , testGroup "Parse"
        [ testParse
        , testParseDefault
        , testWithError
        , testWithErrorX
        , testPrefixError
        , testPrefixErrorX
        , testParseWithRead
        , testParseWithRead'
        , testMaybeParseWithRead
        , testParseEnum
        , testParseEnum'
        , testParseX
        , testParseMaybe
        , testParseMaybeX
        , testParseOrFail
        , testParseOrFailX
        , testParseUnsafe
        , testParseUnsafeX
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
