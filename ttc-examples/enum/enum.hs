------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : example of parseEnum usage
-- Copyright   : Copyright (c) 2019-2024 Travis Cardwell
-- License     : MIT
--
-- This example program prints greetings in multiple languages.  The language
-- is specified using a command line option and is parsed using
-- 'TTC.parseEnum', supporting case-insensitive prefixes.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if defined(MIN_VERSION_ansi_wl_pprint)
#if MIN_VERSION_ansi_wl_pprint (1,0,2)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
#if !MIN_VERSION_optparse_applicative (0,18,0)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)
#endif

-- https://hackage.haskell.org/package/base
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA
#if MIN_VERSION_optparse_applicative (0,18,0)
import Options.Applicative.Help.Pretty (Doc)
#endif

-- https://hackage.haskell.org/package/prettyprinter
#if MIN_VERSION_optparse_applicative (0,18,0)
import qualified Prettyprinter as Doc
#endif

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

-- Supported languages
data Language
  = Chinese
  | English
  | French
  | German
  | Japanese
  | Korean
  | Spanish
  deriving (Bounded, Enum, Eq, Ord, Show)

instance TTC.Parse Language where
  parse = TTC.parseEnum True True
    (TTC.fromS "unknown language")
    (TTC.fromS "ambiguous language")

instance TTC.Render Language where
  render = TTC.renderWithShow

-- Greetings for each language
greeting :: Language -> String
greeting Chinese  = "你好！"
greeting English  = "Hello!"
greeting French   = "Bonjour!"
greeting German   = "Hallo!"
greeting Japanese = "こんにちは！"
greeting Korean   = "안녕하세요!"
greeting Spanish  = "¡Hola!"

------------------------------------------------------------------------------

docString :: String -> Doc
#if MIN_VERSION_optparse_applicative (0,18,0)
docString = Doc.pretty
#else
docString = Doc.string
#endif

------------------------------------------------------------------------------

-- Print a greeting in the specified language
--
-- Languages are usually represented using standard language codes, but
-- English language names are used in this example.
main :: IO ()
main = putStrLn . greeting =<< OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo Language
    pinfo = OA.info (OA.helper <*> langOption) $ mconcat
      [ OA.fullDesc
      , OA.footerDoc $ Just langHelp
      ]

    langOption :: OA.Parser Language
    langOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
      [ OA.long "lang"
      , OA.short 'l'
      , OA.metavar "LANG"
      , OA.value defaultLanguage
      , OA.showDefaultWith TTC.render
      , OA.help "Greeting language"
      ]

    defaultLanguage :: Language
    defaultLanguage = English

    langHelp :: Doc
    langHelp =
      ((docString "Languages:" <> Doc.line) <>) . Doc.indent 2 $ Doc.vcat
        [ docString $ TTC.render lang
        | lang <- [minBound :: Language ..]
        ]
