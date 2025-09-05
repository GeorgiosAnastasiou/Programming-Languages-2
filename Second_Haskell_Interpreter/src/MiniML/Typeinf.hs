{-# LANGUAGE FlexibleContexts #-} 
module MiniML.Typeinf where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when)
import Data.List (nub, (\\))
import MiniML.Syntax ( Type(..), TypeScheme(..), Exp(..), Bop(..), Uop(..), Posn )
import MiniML.Error
import MiniML.Print -- For better error messages
import Debug.Trace -- Debug.Trace is your friend
import qualified MiniML.Lex as Alex 

type Substitution = M.Map String Type
type Ctx = M.Map String TypeScheme

-- add substitution to state
data TypeInfState = MkTIState {
  fresh :: Int,
  subst :: Substitution
}

--debugM :: String -> TypeInf ()
--debugM msg = trace ("[DEBUG] " ++ msg) (return ())

type TypeInf = StateT TypeInfState (Except (Posn, String))

noPos :: Posn
noPos = Alex.AlexPn 0 0 0

applySubst :: Type -> Substitution -> Type
applySubst TUnit _ = TUnit
applySubst TInt _ = TInt
applySubst TBool _ = TBool
applySubst (TVar a) s = case M.lookup a s of
  Nothing -> TVar a
  Just t -> t
applySubst (TArrow t1 t2) s = TArrow (applySubst t1 s) (applySubst t2 s)
applySubst (TProd t1 t2) s = TProd (applySubst t1 s) (applySubst t2 s)
applySubst (TSum t1 t2) s = TSum (applySubst t1 s) (applySubst t2 s)
applySubst (TList t) s = TList (applySubst t s)

-- empty substitution
initState :: TypeInfState
initState = MkTIState 0 M.empty

-- fresh vars 
freshTVar :: TypeInf Type
freshTVar = do
  n <- gets fresh
  let name = "a" ++ show n
  modify $ \s -> s { fresh = n + 1 }
  return (TVar name)

-- Substitution composition
compose :: Substitution -> Substitution -> Substitution
compose sNew sOld = 
  M.map (\t -> applySubst t sNew) sOld `M.union` sNew


-- Type scheme handling
instantiate :: TypeScheme -> TypeInf Type
instantiate (Type t) = return t
instantiate (Forall vars t) = do
  freshVars <- mapM (const freshTVar) vars
  let subst = M.fromList (zip vars freshVars)
  return $ applySubst t subst

generalize :: Ctx -> Type -> TypeScheme
generalize ctx t =
  let ctxVars = freeInContext ctx
      typeVars = freeVars t
      polyVars = typeVars \\ ctxVars
  in Forall polyVars t

freeInContext :: Ctx -> [String]
freeInContext ctx = nub $ concatMap freeInScheme (M.elems ctx)
  where
    freeInScheme (Type t) = freeVars t
    freeInScheme (Forall vars t) = freeVars t \\ vars

-- Substitution application
applySubstCtx :: Ctx -> Substitution -> Ctx
applySubstCtx ctx subst = M.map (applySubstScheme subst) ctx
  where
    applySubstScheme s (Type t) = Type (applySubst t s)
    applySubstScheme s (Forall vars t) =
      let s' = foldr M.delete s vars
      in Forall vars (applySubst t s')


inferTypeTop :: Exp -> Either (Posn, String) TypeScheme
inferTypeTop e = runExcept $ evalStateT action initState
  where
    action = do
      (s, ty) <- infer M.empty e
      currentSubst <- gets subst
      let finalTy = applySubst ty currentSubst
      case e of
        Abs{} -> return $ Type finalTy
        _     -> return $ generalize M.empty finalTy


-- trying to split types that polymorphism applies to vs ones that it doesnt, earlier
--isNonExpansive :: Exp -> Bool
--isNonExpansive expr = case expr of
--  Var _ _           -> True
--  Abs _ _ _ _ _     -> True
--  NumLit _ _        -> True
--  BoolLit _ _       -> True
--  Unit _            -> True
--  Nil _             -> True
--  Pair _ e1 e2      -> isNonExpansive e1 && isNonExpansive e2
--  Inl _ e           -> isNonExpansive e
--  Inr _ e           -> isNonExpansive e
--  Cons _ hd tl      -> isNonExpansive hd && isNonExpansive tl
--  -- Everything else is considered expansive:
--  _                 -> False

applySubstScheme :: Substitution -> TypeScheme -> TypeScheme
applySubstScheme s (Type t) = Type (applySubst t s)
applySubstScheme s (Forall vars t) =
  let s' = foldr M.delete s vars
  in Forall vars (applySubst t s')

-- New unification that modifies state
unify :: Type -> Type -> Posn -> TypeInf ()
unify t1 t2 pos = do
  currentSubst <- gets subst
  let t1' = applySubst t1 currentSubst
      t2' = applySubst t2 currentSubst
  
  case (t1', t2') of
    (TVar a, TVar b) | a == b -> return ()
    
    (TVar a, _) -> do
      when (occursCheck a t2') $
        throwError (pos, "Occurs check: " ++ a ++ " in " ++ showType t2')
      modifySubst (M.insert a t2')
      --debugM ("Updated substitution with " ++ a ++ " -> " ++ show t2')
    
    (_, TVar b) -> unify t2' t1' pos
    
    (TArrow t1 t2, TArrow t3 t4) -> do
      unify t1 t3 pos
      unify t2 t4 pos
    
    (TProd t1 t2, TProd t3 t4) -> do
      unify t1 t3 pos
      unify t2 t4 pos
    
    (TSum t1 t2, TSum t3 t4) -> do
      unify t1 t3 pos
      unify t2 t4 pos
    
    (TList t1, TList t2) -> unify t1 t2 pos
    
    (TUnit, TUnit) -> return ()
    (TInt, TInt) -> return ()
    (TBool, TBool) -> return ()
    
    _ -> throwError (pos, "Type mismatch: " ++ showType t1' ++ " vs " ++ showType t2')
  where
    modifySubst f = modify (\s -> s { subst = f (subst s) })
    occursCheck a t = a `elem` freeVars t

--elper functions
freeVars :: Type -> [String]
freeVars = nub . go
  where
    go (TVar a) = [a]
    go (TArrow t1 t2) = go t1 ++ go t2
    go (TProd t1 t2) = go t1 ++ go t2
    go (TSum t1 t2) = go t1 ++ go t2
    go (TList t) = go t
    go _ = []


-- New type inference signature
infer :: Ctx -> Exp -> TypeInf (Substitution, Type)
infer ctx exp = do
  --debugM ("infer: " ++ show exp ++ " with ctx: " ++ show ctx) 
  case exp of
    -- Variable case
    Var pos x -> case M.lookup x ctx of
      Nothing -> throwError (pos, "Unbound variable: " ++ x)
      Just scheme -> do
        currentSubst <- gets subst
        t <- instantiate (applySubstScheme currentSubst scheme)
        return (M.empty, t)

--    from how the poly.ml looked, i thought Uop Not contains both the 'Not' and the 'Neg'
--    and I thought I can include then in a 'case' statement for which one it would be, but that seems to have been wrong.

    Uop pos op expr -> case op of -- fainetai ligo xazo na xrhsimopoiw 'case' gia ena dedomeno alla den peirazei, eida sto
-- Parse kai sto Syntax oti to ~ einai to Not, kai ap oti katalava xrhsimopoieitai k se int? -- telika oxi
--
      Not -> do
        (s, t) <- infer ctx expr
        unify t TBool pos
        return (s, TBool)
        --resulttype <- freshTVar
        --unify t resulttype pos
        --return (s, resulttype)    

    -- Lambda abstraction
    
    Abs pos x maybeType _ body -> do
      argTy <- case maybeType of
        Just t -> return t
        Nothing -> freshTVar
      
      let newCtx = M.insert x (Type argTy) ctx
      (s1, bodyTy) <- infer newCtx body
      return (s1, TArrow (applySubst argTy s1) (applySubst bodyTy s1))

    -- Application
    App pos fn arg -> do
      (s1, fnTy) <- infer ctx fn
      let ctx' = applySubstCtx ctx s1
      (s2, argTy) <- infer ctx' arg
      resTy <- freshTVar
      let fnTy' = applySubst fnTy s2
      unify fnTy' (TArrow argTy resTy) pos
      currentSubst <- gets subst
      return (currentSubst `compose` s2 `compose` s1, applySubst resTy currentSubst)

    -- Let binding
    Let pos x _ e1 e2 -> do
      (s1, t1) <- infer ctx e1
      currentSubst <- gets subst --1
      let ctx' = applySubstCtx ctx s1
          t1' = applySubst t1 currentSubst --2 
          generalized = generalize ctx' t1' -- t1-> t1'
          newCtx = M.insert x generalized ctx'
      (s2, t2) <- infer newCtx e2
      return (s2 `compose` s1, t2)

 --   Let pos x _ e1 e2 -> do
 --     (s1, t1) <- infer ctx e1
 --     let ctx' = applySubstCtx ctx s1
 --         generalized = if isNonExpansive e1 then generalize ctx' t1 else Type t1
 --         newCtx = M.insert x generalized ctx'
 --     (s2, t2) <- infer newCtx e2
 --     return (s2 `compose` s1, t2)

    -- Literals
    NumLit _ _ -> return (M.empty, TInt)
    BoolLit _ _ -> return (M.empty, TBool)
    Unit _ -> return (M.empty, TUnit)

    -- If-then-else
    ITE pos cond thn els -> do
      (s1, condTy) <- infer ctx cond
      unify condTy TBool pos
      (s2, thnTy) <- infer (applySubstCtx ctx s1) thn
      (s3, elsTy) <- infer (applySubstCtx ctx (s2 `compose` s1)) els
      unify (applySubst thnTy s3) (applySubst elsTy s3) pos
      finalSubst <- gets subst
      return (finalSubst `compose` s3 `compose` s2 `compose` s1, applySubst thnTy finalSubst)





    -- Pair constructor
    Pair pos e1 e2 -> do
      (s1, t1) <- infer ctx e1
      (s2, t2) <- infer (applySubstCtx ctx s1) e2
      return (s2 `compose` s1, TProd (applySubst t1 s2) (applySubst t2 s2))

    -- First projection
    Fst pos e -> do
      (s1, t) <- infer ctx e
      ty1 <- freshTVar
      ty2 <- freshTVar
      unify t (TProd ty1 ty2) pos
      currentSubst <- gets subst
      return (currentSubst `compose` s1, applySubst ty1 currentSubst)

    -- Second projection
    Snd pos e -> do
      (s1, t) <- infer ctx e
      ty1 <- freshTVar
      ty2 <- freshTVar
      unify t (TProd ty1 ty2) pos
      currentSubst <- gets subst
      return (currentSubst `compose` s1, applySubst ty2 currentSubst)



    -- Inleft constructor
    Inl pos e -> do
      (s, t) <- infer ctx e
      rightTy <- freshTVar
      return (s, TSum (applySubst t s) rightTy)

    -- Inright constructor
    Inr pos e -> do
      (s, t) <- infer ctx e
      leftTy <- freshTVar
      return (s, TSum leftTy (applySubst t s))

    -- Case expression for sums
    Case pos scrut x1 branch1 x2 branch2 -> do
      (s1, scrutTy) <- infer ctx scrut
      leftTy <- freshTVar
      rightTy <- freshTVar
      unify scrutTy (TSum leftTy rightTy) pos
      
      let ctx1 = applySubstCtx ctx s1
          ctxLeft = M.insert x1 (Type leftTy) ctx1
          ctxRight = M.insert x2 (Type rightTy) ctx1
      
      (s2, t1) <- infer ctxLeft branch1
      (s3, t2) <- infer ctxRight branch2
      
      unify (applySubst t1 s3) (applySubst t2 s3) pos
      finalSubst <- gets subst
      return (finalSubst `compose` s3 `compose` s2 `compose` s1, 
              applySubst t1 finalSubst)

    -- Empty list
    Nil pos -> do
      elemTy <- freshTVar
      return (M.empty, TList elemTy)

    -- Cons constructor
    Cons pos hd tl -> do
      (s1, hdTy) <- infer ctx hd
      (s2, tlTy) <- infer (applySubstCtx ctx s1) tl
      elemTy <- freshTVar
      unify tlTy (TList elemTy) pos
      unify (applySubst hdTy s2) elemTy pos
      currentSubst <- gets subst
      return (currentSubst `compose` s2 `compose` s1, 
              TList (applySubst elemTy currentSubst))

    -- List case expression
    CaseL pos scrut nilCase x xs consCase -> do
      (s1, scrutTy) <- infer ctx scrut
      elemTy <- freshTVar
      unify scrutTy (TList elemTy) pos
      
      let ctx' = applySubstCtx ctx s1
          ctxNil = ctx'
          ctxCons = M.insert x (Type elemTy) 
                 . M.insert xs (Type (TList elemTy)) $ ctx'
      
      (s2, nilTy) <- infer ctxNil nilCase
      (s3, consTy) <- infer ctxCons consCase
      
      unify (applySubst nilTy s3) (applySubst consTy s3) pos
      finalSubst <- gets subst
      return (finalSubst `compose` s3 `compose` s2 `compose` s1,
              applySubst nilTy finalSubst)


  --  LetRec pos f x maybeArgTy maybeRetTy e1 e2 -> do
  --    -- Create fresh variables for the function type
  --    let tempCtx = M.insert f (Type (TArrow TInt TInt))
  --               . M.insert x (Type TInt) $ ctx
  --    argTy <- case maybeArgTy of
  --      Just t -> return t
  --      Nothing -> freshTVar
  --    resTy <- case maybeRetTy of
  --      Just t -> return t
  --      Nothing -> freshTVar
  --    
  --    -- Create function type and temporary scheme
  --    let funcTy = TArrow argTy resTy
  --        tempCtx = M.insert f (Type funcTy) 
  --               . M.insert x (Type argTy) $ ctx
  --    
  --    -- Infer body with temporary typing context
  --    (s1, bodyTy) <- infer tempCtx e1
  --    unify (applySubst resTy s1) (applySubst bodyTy s1) pos
  --    
  --    -- Generalize after unification
  --    currentSubst <- gets subst
  --    let generalizedFuncTy = applySubst funcTy currentSubst
  --        genCtx = applySubstCtx ctx currentSubst
  --        fScheme = generalize (M.delete f genCtx) generalizedFuncTy
  --    
  --    -- Infer rest with generalized scheme
  --    let finalCtx = M.insert f fScheme genCtx
  --    (s2, finalTy) <- infer finalCtx e2
  --    
  --    return (s2 `compose` currentSubst `compose` s1, finalTy)         
    LetRec pos f x _maybeArgTy _maybeRetTy e1 e2 -> do
      argTy <- freshTVar
      resTy <- freshTVar
      let funcTy = TArrow argTy resTy
          tempCtx = M.insert f (Type funcTy) 
                 . M.insert x (Type argTy) $ ctx
      (s1, bodyTy) <- infer tempCtx e1
      unify (applySubst resTy s1) (applySubst bodyTy s1) pos
      currentSubst <- gets subst
      let finalFuncTy = applySubst funcTy (currentSubst `compose` s1)
      let genCtx = applySubstCtx ctx (currentSubst `compose` s1)
          --fScheme = generalize (M.delete f genCtx) finalFuncTy
          fScheme = Type finalFuncTy
      let finalCtx = M.insert f fScheme genCtx
      (s2, finalTy) <- infer finalCtx e2
      return (s2 `compose` currentSubst `compose` s1, finalTy)

    -- Binary operations
    Bop pos op e1 e2 -> case op of
      Plus -> arithOp pos TInt
      Minus -> arithOp pos TInt
      Mult -> arithOp pos TInt
      Div -> arithOp pos TInt
      And -> boolOp pos
      Or -> boolOp pos
      Lt -> compOp pos TInt
      Le -> compOp pos TInt
      Gt -> compOp pos TInt
      Ge -> compOp pos TInt
      Eq -> eqOp pos
      where
        arithOp pos ty = do
          (s1, t1) <- infer ctx e1
          (s2, t2) <- infer (applySubstCtx ctx s1) e2
          unify (applySubst t1 s2) TInt pos
          unify (applySubst t2 s2) TInt pos
          return (s2 `compose` s1, TInt) -- edw profanws einai int gia arithmetic OP 

        boolOp pos = do
          (s1, t1) <- infer ctx e1
          (s2, t2) <- infer (applySubstCtx ctx s1) e2
          unify (applySubst t1 s2) TBool pos
          unify (applySubst t2 s2) TBool pos
          return (s2 `compose` s1, TBool)

        compOp pos ty = do
          (s1, t1) <- infer ctx e1
          (s2, t2) <- infer (applySubstCtx ctx s1) e2
          unify (applySubst t1 s2) ty pos
          unify (applySubst t2 s2) ty pos
          return (s2 `compose` s1, TBool)

        eqOp pos = do
          (s1, t1) <- infer ctx e1
          (s2, t2) <- infer (applySubstCtx ctx s1) e2
          let t1' = applySubst t1 s2
              t2' = applySubst t2 s2
          -- allow comparison of any type except functions
          when (isFunctionType t1' || isFunctionType t2') $
            throwError (pos, "Cannot compare function types")
          --unify t1' TInt pos
          unify t1' t2' pos
          return (s2 `compose` s1, TBool)

    _ -> throwError (noPos, "Unhandled expression type")

isFunctionType :: Type -> Bool
isFunctionType (TArrow _ _) = True
isFunctionType _ = False


