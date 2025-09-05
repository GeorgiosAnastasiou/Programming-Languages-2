module Gen where

import Test.QuickCheck
import Control.Monad
import qualified Data.Map as M
import MiniML.Syntax

genTypeSize :: Int -> Gen Type
genTypeSize 0 = elements [TInt, TBool, TUnit]
genTypeSize s = 
  let genTypeS = genTypeSize (s - 1)
  in frequency
    [ (3, elements [TInt, TBool, TUnit])
    , (2, liftM2 TArrow genTypeS genTypeS)
    , (1, liftM2 TSum genTypeS genTypeS)
    , (1, liftM2 TProd genTypeS genTypeS)
    , (1, TRef <$> genTypeS)
    ]

genExp :: Gen Exp
genExp = scale (min 20) $ sized genExpSize

genType :: Gen Type
genType = scale (min 10) $ sized genTypeSize

genBop :: Gen Bop
genBop = elements [Plus, Minus, Mult, Div, And, Or, Lt, Gt, Le, Ge, Eq]

genVar :: Gen String
genVar = do
  n <- chooseInt (1, 200)
  x <- elements ["x", "y", "z", "test_42", "foo_", "_bar", "z21", "f", "g", "lala"]
  return (x ++ show n)

freshName :: Int -> Gen String
freshName counter = return ("x" ++ show counter)

genTExpSize :: M.Map Type [String] -> Int -> Type -> Int -> Gen Exp
genTExpSize env counter typ size =
  case size of
    0 -> genBaseCase env typ
    n -> frequency
      [ (3, genBaseCase env typ)
      , (1, genComposite env counter typ (n-1))
      ]

genExpType :: Gen (Exp, Type)
genExpType = do
  typ <- genType
  expr <- genExpOfType typ
  return (expr, typ)

genExpOfType :: Type -> Gen Exp
genExpOfType t = sized $ \s -> do
  let initialEnv = M.fromList [(t, ["v1", "v2", "v3"])]
  genTExpSize initialEnv 0 t (min 5 s)


genVarOfType :: M.Map Type [String] -> Type -> Gen Exp
genVarOfType env typ =
  case M.lookup typ env of
    Just vars | not (null vars) -> elements (map (Var nowhere) vars)
    _ -> fallbackLiteral typ
  where
    fallbackLiteral TBool = oneof [ return (BoolLit nowhere True)
                                  , return (BoolLit nowhere False) ]
    fallbackLiteral TInt  = liftM (NumLit nowhere) arbitrary
    fallbackLiteral TUnit = return (Unit nowhere)
    fallbackLiteral _     = error "No literal available for this type"


genExpSize :: Int -> Gen Exp
genExpSize s = case s of
    0 -> baseCases
    _ -> frequency [ (2, baseCases)
                   , (1, liftM2 (App nowhere) genExpS genExpS)
                   , (1, liftM4 (Abs nowhere) genVar genTypeS (return Nothing) genExpS)
                   , (1, liftM3 (ITE nowhere) genExpS genExpS genExpS)
                   , (1, liftM3 (Bop nowhere) genBop genExpS genExpS)
                   , (1, liftM  (Uop nowhere Not) genExpS)
                   , (1, liftM2 (Pair nowhere) genExpS genExpS)
                   , (1, liftM  (Fst nowhere) genExpS)
                   , (1, liftM  (Snd nowhere) genExpS)
                   , (1, liftM2 (Inl nowhere) genTypeS genExpS)
                   , (1, liftM2 (Inr nowhere) genTypeS genExpS)
                   , (1, liftM5 (Case nowhere) genExpS genVar genExpS genVar genExpS)
                   , (1, liftM4 (Let nowhere) genVar genTypeS genExpS genExpS)
                   , (1, do
                           x <- genVar
                           liftM5 (LetRec nowhere x) genVar genTypeS genTypeS genExpS genExpS)
                   , (1, liftM2 (Asgn nowhere) genExpS genExpS)
                   , (1, liftM  (Deref nowhere) genExpS)
                   , (1, liftM  (Ref nowhere) genExpS)
                   ]
  where
    genExpS = genExpSize (s-1)
    genTypeS = genTypeSize (s-1)
    baseCases = oneof [ return (Unit nowhere)
                      , liftM (NumLit nowhere) arbitrary
                      , liftM (BoolLit nowhere) arbitrary 
		      ] 


genBaseCase :: M.Map Type [String] -> Type -> Gen Exp
genBaseCase env typ = oneof
  [ genVarOfType env typ
  , case typ of
      TInt  -> NumLit nowhere <$> arbitrary
      TBool -> BoolLit nowhere <$> arbitrary
      TUnit -> return (Unit nowhere)
      _     -> error "No literal for complex type"
  ]


genComposite :: M.Map Type [String] -> Int -> Type -> Int -> Gen Exp
genComposite env c (TArrow t1 t2) s = do
  x <- freshName c
  body <- genTExpSize (M.insertWith (++) t1 [x] env) (c+1) t2 s
  return (Abs nowhere x t1 (Just t2) body)

genComposite env c (TProd t1 t2) s = 
  Pair nowhere <$> genTExpSize env c t1 s <*> genTExpSize env c t2 s

genComposite env c (TSum t1 t2) s =
  oneof [ Inl nowhere t2 <$> genTExpSize env c t1 s
        , Inr nowhere t1 <$> genTExpSize env c t2 s ]



