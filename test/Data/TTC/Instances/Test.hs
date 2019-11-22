module Data.TTC.Instances.Test (tests) where

-- https://hackage.haskell.org/package/base
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- (ttc)
import qualified Data.TTC as TTC
import Data.TTC.Instances ()

{-# ANN module "HLint: ignore Reduce duplication" #-}

------------------------------------------------------------------------------
-- $TestData

xS :: String
xS = "test テスト"

xT :: T.Text
xT = TTC.convert "test テスト"

xTL :: TL.Text
xTL = TTC.convert "test テスト"

xBS :: BS.ByteString
xBS = TTC.convert "test テスト"

xBSL :: BSL.ByteString
xBSL = TTC.convert "test テスト"

------------------------------------------------------------------------------

testChar :: TestTree
testChar = testGroup "Char"
    [ testCase "Render" $ "*" @=? TTC.render '*'
    , testCase "Parse.OK" $ Right '*' @=? TTC.parse "*"
    , testCase "Parse.empty" $ Left "invalid Char" @=?
        (TTC.parse "" :: Either String Char)
    , testCase "Parse.multiple" $ Left "invalid Char" @=?
        (TTC.parse "**" :: Either String Char)
    ]

testDouble :: TestTree
testDouble = testGroup "Double"
    [ testCase "Render" $ s @=? TTC.render x
    , testCase "Parse.OK" $ Right x' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Double" @=?
        (TTC.parse "invalid" :: Either String Double)
    ]
  where
    x :: Double
    x = 3.14159

    s :: String
    s = show x

    x' :: Double
    x' = read s

testFloat :: TestTree
testFloat = testGroup "Float"
    [ testCase "Render" $ s @=? TTC.render x
    , testCase "Parse.OK" $ Right x' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Float" @=?
        (TTC.parse "invalid" :: Either String Float)
    ]
  where
    x :: Float
    x = 3.14159

    s :: String
    s = show x

    x' :: Float
    x' = read s

testInt :: TestTree
testInt = testGroup "Int"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Int" @=?
        (TTC.parse "invalid" :: Either String Int)
    ]
  where
    n :: Int
    n = 42

    s :: String
    s = show n

    n' :: Int
    n' = read s

testInt8 :: TestTree
testInt8 = testGroup "Int8"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Int8" @=?
        (TTC.parse "invalid" :: Either String Int8)
    ]
  where
    n :: Int8
    n = 42

    s :: String
    s = show n

    n' :: Int8
    n' = read s

testInt16 :: TestTree
testInt16 = testGroup "Int16"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Int16" @=?
        (TTC.parse "invalid" :: Either String Int16)
    ]
  where
    n :: Int16
    n = 42

    s :: String
    s = show n

    n' :: Int16
    n' = read s

testInt32 :: TestTree
testInt32 = testGroup "Int32"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Int32" @=?
        (TTC.parse "invalid" :: Either String Int32)
    ]
  where
    n :: Int32
    n = 42

    s :: String
    s = show n

    n' :: Int32
    n' = read s

testInt64 :: TestTree
testInt64 = testGroup "Int64"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Int64" @=?
        (TTC.parse "invalid" :: Either String Int64)
    ]
  where
    n :: Int64
    n = 42

    s :: String
    s = show n

    n' :: Int64
    n' = read s

testInteger :: TestTree
testInteger = testGroup "Integer"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Integer" @=?
        (TTC.parse "invalid" :: Either String Integer)
    ]
  where
    n :: Integer
    n = 42

    s :: String
    s = show n

    n' :: Integer
    n' = read s

testWord :: TestTree
testWord = testGroup "Word"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Word" @=?
        (TTC.parse "invalid" :: Either String Word)
    ]
  where
    n :: Word
    n = 42

    s :: String
    s = show n

    n' :: Word
    n' = read s

testWord8 :: TestTree
testWord8 = testGroup "Word8"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Word8" @=?
        (TTC.parse "invalid" :: Either String Word8)
    ]
  where
    n :: Word8
    n = 42

    s :: String
    s = show n

    n' :: Word8
    n' = read s

testWord16 :: TestTree
testWord16 = testGroup "Word16"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Word16" @=?
        (TTC.parse "invalid" :: Either String Word16)
    ]
  where
    n :: Word16
    n = 42

    s :: String
    s = show n

    n' :: Word16
    n' = read s

testWord32 :: TestTree
testWord32 = testGroup "Word32"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Word32" @=?
        (TTC.parse "invalid" :: Either String Word32)
    ]
  where
    n :: Word32
    n = 42

    s :: String
    s = show n

    n' :: Word32
    n' = read s

testWord64 :: TestTree
testWord64 = testGroup "Word64"
    [ testCase "Render" $ s @=? TTC.render n
    , testCase "Parse.OK" $ Right n' @=? TTC.parse s
    , testCase "Parse.invalid" $ Left "invalid Word64" @=?
        (TTC.parse "invalid" :: Either String Word64)
    ]
  where
    n :: Word64
    n = 42

    s :: String
    s = show n

    n' :: Word64
    n' = read s

testString :: TestTree
testString = testGroup "String"
    [ testCase "Render" $ xS @=? TTC.render xS
    , testCase "Parse.empty" $ Right "" @=? TTC.parse ""
    , testCase "Parse.nonempty" $ Right xS @=? TTC.parse xS
    ]

testBSL :: TestTree
testBSL = testGroup "BSL.ByteString"
    [ testCase "Render" $ xS @=? TTC.render xBSL
    , testCase "Parse.empty" $ Right BSL.empty @=? TTC.parse ""
    , testCase "Parse.nonempty" $ Right xBSL @=? TTC.parse xS
    ]

testBS :: TestTree
testBS = testGroup "BS.ByteString"
    [ testCase "Render" $ xS @=? TTC.render xBS
    , testCase "Parse.empty" $ Right BS.empty @=? TTC.parse ""
    , testCase "Parse.nonempty" $ Right xBS @=? TTC.parse xS
    ]

testTL :: TestTree
testTL = testGroup "TL.Text"
    [ testCase "Render" $ xS @=? TTC.render xTL
    , testCase "Parse.empty" $ Right TL.empty @=? TTC.parse ""
    , testCase "Parse.nonempty" $ Right xTL @=? TTC.parse xS
    ]

testT :: TestTree
testT = testGroup "T.Text"
    [ testCase "Render" $ xS @=? TTC.render xT
    , testCase "Parse.empty" $ Right T.empty @=? TTC.parse ""
    , testCase "Parse.nonempty" $ Right xT @=? TTC.parse xS
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Data.TTC.Instances"
    [ testChar
    , testDouble
    , testFloat
    , testInt
    , testInt8
    , testInt16
    , testInt32
    , testInt64
    , testInteger
    , testWord
    , testWord8
    , testWord16
    , testWord32
    , testWord64
    , testString
    , testBSL
    , testBS
    , testTL
    , testT
    ]
