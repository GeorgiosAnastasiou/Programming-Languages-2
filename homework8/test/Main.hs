module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import MiniML
import Gen

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import MiniML.Parse (parse) -- To parse MiniML code strings
import MiniML.Lex (lexer)   -- To lex MiniML code strings
import MiniML.Typeinf (inferTypeTop, showTypeScheme) -- To use your type inference functions and show types
import MiniML.Error (Error)
import MiniML.Syntax (TypeScheme, Type(..)) -- To define expected types and handle TypeScheme
import Text.ParserCombinators.Parsec (parse) -- Make sure to import the correct `parse`

-- The main testing function. Runs a series of tests. Add as many additional
-- tests as you want.

main :: IO ()
main = defaultMain $ testGroup "act"
  [ testProperty "Parser round trip" parserRoundTrip
  , testProperty "Type inference" testTypeInf
  , testProperty "Placeholder" someProperty ]

-- A property that for a randomly MiniML as pretty-printing it and parsing it
-- produces the original program (i.e., `parse . lex . showExp == id`)
parserRoundTrip :: Property
parserRoundTrip =
  forAll genExp (\e ->
    let txt = showExp e in
    case parse $ lexer txt of
      Left err -> whenFail (prettyErrs txt err) (counterexample "Parsing failed!" False)
      Right e' -> e === e')


testTypeInf :: Property
testTypeInf = counterexample "FILL IN HERE!" False -- ok i need to add stuff here


someProperty :: Property
someProperty =
  forAll (choose (5,10 :: Int)) (<= 10)
