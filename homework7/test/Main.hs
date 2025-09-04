module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck
import MiniML
import Gen


main :: IO ()
main = defaultMain $ testGroup "MiniML Tests"
  [ testProperty "Parser round trip" parserRoundTrip
  , testProperty "Well-typed expressions" genTExpWellTyped
  , testProperty "Type soundness" typeSoundness
  , testProperty "Placeholder" someProperty ] 

parserRoundTrip :: Property
parserRoundTrip =
  forAll genExp $ \e ->
    case parse (lexer (showExp e)) of
      Left err -> counterexample ("Parse error: " ++ show err) False
      Right e' -> e === e'


--genTExpWellTyped :: Property
--genTExpWellTyped = forAll genExpType $ \(e, t) ->
--  case typeCheckTop e of
--    Left err -> counterexample ("Type error: " ++ show err) False
--    Right t' -> counterexample ("Expected type: " ++  showType t ++ "but got " ++ showType t') (t == t') 
--
--
--typeSoundness = forAll genType $ \t ->
--  forAll (genExpOfType t) $ \e ->
--    case typeCheckTop e of
--      Right _ ->
--        case evalTop e of
--          Left _ -> True
--          Right (v, s) ->
--            case typeCheckVTop s v of
--              Right t' -> t == t'
--              Left _ -> False
--      Left _ -> False 


genTExpWellTyped :: Property
genTExpWellTyped = forAll genExpType $ \(e, t) ->
  case typeCheckTop e of 
    Left err -> counterexample ("Type error: " ++ show err) False
    Right t' -> counterexample ("Expected type: " ++ showType t ++ " but got " ++ showType t') (t == t')

typeSoundness :: Property
typeSoundness = forAll genType $ \t ->
  forAll (genExpOfType t) $ \e ->
    case typeCheckWithType t e of 
      Right _ ->
        case evalWithType t e of 
          Left _ -> True
          Right (v, s) ->
            case typeCheckVTop s v of
              Right t' -> t == t'
              Left _ -> False
      Left _ -> False

someProperty = forAll (choose (5,10 :: Int)) (<= 10)
