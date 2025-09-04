module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = hUnitTestToTests $ TestList
  [ testCase "Minimal Test" testMinimal
  ]

testMinimal :: Assertion
testMinimal = assertBool "Minimal test always passes" True 
