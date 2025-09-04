-- module MiniML.Eval where
module MiniML.Eval (eval, evalTop, evalWithType) where

import qualified Data.Map as M
import Control.Monad.State
-- import Data.Fix (fix) -- as F
import Control.Monad.Fix (mfix)
import MiniML.Syntax
import MiniML.Error
import MiniML.Values


-- MiniML evaluation

-- Make sure to look at Values.hs for the associated data types.

-- The evaluation state.
type EvalState = ( Env     -- An environment mapping variables to their values.
                 , Store   -- A store mapping memory locations to values.
                 , Loc     -- The next available memory location. Needed when allocation new references.
                 )

-- The type of the evaluation computation.
type Eval = StateT EvalState (Either (Posn,String))

-- `StateT` is the monad transformer for the State monad. It allows you to put
-- an arbitrary monad inside the State. Here, we put an Error monad inside the
-- result of the state, composing the State monad with the Error monad.

-- The resulting monad, `Eval`, manages both mutable state and error propagation
-- within a single monad.

-- Essentially, the type `Eval a` represents a computation of type `EvalState -> (EvalState, Error a)`

-- Note 1: In the definition of `Eval`, we use `Either (Posn, String)` directly
-- instead of the type synonym `Error` (defined in Error.hs) because Haskell
-- does not allow partially applied type synonyms.

-- Note 2: Technically, we could have used a reader monad for Env, but it makes
-- our definitions simpler to put it in the state and avoid composing too many
-- monads.

-- Useful functions for handling the state.

-- Get the environment
getEnv :: Eval Env
getEnv = do
  (env, _, _) <- get
  return env

-- Update the environment
putEnv :: Env -> Eval ()
putEnv env = do
  (_, store, l) <- get
  put (env, store, l)

-- Get the store
getStore :: Eval Store
getStore = do
  (_, store, _) <- get
  return store

-- Update the store
putStore :: Store -> Eval ()
putStore store = do
  (env, _, l) <- get
  put (env, store, l)

-- Run a computation in the provided environment
localEnv :: Env -> Eval a -> Eval a
localEnv env m = do
  env' <- getEnv
  putEnv env
  x <- m
  putEnv env'
  return x

-- Update the store using the given function and run a computation
withStore :: (Store -> Store) -> Eval a -> Eval a
withStore f m = do
  store <- getStore
  putStore (f store)
  m

-- Return a fresh location and increase the location counter
freshLoc :: Eval Loc
freshLoc = do
  (env, store, l) <- get
  let l' = l + 1
  put (env, store, l')
  return l'

-- Throw an error.
throwErr :: Posn -> String -> Eval a
throwErr p str = lift (throw (p,str))

-- Main evaluation function.

-- TODO 2: Fill in the definition for the evaluation function.

-- Make sure to correctly propagate the types to closure values. This should be
-- available as type annotations in the input program (Do not use the
-- typechecking function in te evaluation function!). You can assume that the
-- input programs will never have the form `Abs p x t Nothing e` (i.e., return
-- type annotations will be filled).

eval :: Exp -> Eval Value

eval (Var p x) = do --
  (env, _, _) <- get
  case M.lookup x env of
    Just v -> return v
    Nothing -> throwErr p ("Unbound bariable: " ++ x)
    -- _ -> throwErr p "Other error" -- den xreiazetai



eval (NumLit _ n) = return (VNum n)

eval (BoolLit _ b) = return (VBool b)

eval (Unit _) = return VUnit

eval (Abs p x tArg maybeTRes body) =
  case maybeTRes of
    Nothing -> throwErr p "Missing return type annotation in abstraction"
    Just tRes -> do
      (env, _, _) <- get -- pare to env apo to evalstate, thelei return
      return (VClo env "" x tArg tRes body)


eval (App p e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    VClo closEnv f x _ _ body -> do
      let newEnv = M.insert x v2 closEnv
          newEnv' = if f /= "" then M.insert f v1 newEnv else newEnv
      oldState <- get
      modify (\(_, store, loc) -> (newEnv', store, loc))
      result <- eval body
      put oldState
      return result
    _ -> throwErr p "Application of a non-function value"

-- Let
eval (Let p x t e1 e2) = do
  v1 <- eval e1
  modify (\(env, store, loc) -> (M.insert x v1 env, store, loc))
  eval e2

-- Letrec
--eval (LetRec p f x t1 t2 e1 e2) = do
  --(env, store, loc) <- get
  --let closure = \c -> return (VClo (M.insert f c env) f x t1 t2 e1)
  --closure closure

  --closure <- mfix $ \c -> return (VClo (M.insert f c env) f x t1 t2 e1)
  --modify (\(env, store, loc) -> (M.insert f closure env, store, loc))
  --eval e2
  --do
 --   (env, store, loc) <- get
 --   let closure = \c -> return (VClo (M.insert f c env) f x t1 t2 e1)  -- closure is of type Value -> StateT EvalState (Either (Posn, String)) Value
    -- closure closure  -- Apply closure to itself, but we must ensure closure has the right type.
    --let closure = VClo (M.insert f (VClo ... ) env) f x t1 t2 e1



    --modify (\(env, store, loc) -> (M.insert f closure env, store, loc))
    --return closure 


eval (LetRec p f x t1 t2 e1 e2) = do
  (env, store, loc) <- get
  closure <- mfix $ \c -> return (VClo (M.insert f c env) f x t1 t2 e1)
  modify (\(env, store, loc) -> (M.insert f closure env, store, loc))
  eval e2










-- bop eq

eval (Bop p Eq e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> return (VBool (b1 == b2)) -- mallon?
    (VNum n1, VNum n2) -> return (VBool (n1 == n2))
    _ -> throwErr p "expects either two nums or two bools"



-- If-then-else
eval (ITE p cond eThen eElse) = do
  vCond <- eval cond
  case vCond of
    VBool True  -> eval eThen
    VBool False -> eval eElse
    _           -> throwErr p "Condition of if-then-else is not a boolean"

-- Bin
eval (Bop p Plus e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return (VNum (n1 + n2))
    _ -> throwErr p "Plus expects two numbers"

eval (Bop p Minus e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return (VNum (n1 - n2))
    _ -> throwErr p "Minus expects two numbers"

eval (Bop p Mult e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return (VNum (n1 * n2))
    _ -> throwErr p "Multiplication expects two numbers"

eval (Bop p Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) ->
      if n2 == 0
        then throwErr p "Division by zero"
        else return (VNum (n1 `div` n2))
    _ -> throwErr p "Division expects two numbers"

eval (Bop p And e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> return (VBool (b1 && b2))
    _ -> throwErr p "And expects two booleans"

eval (Bop p Or e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> return (VBool (b1 || b2))
    _ -> throwErr p "Or expects two booleans"



-- BOP for lt gt le ge .. 

eval (Bop p Lt e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return ( VBool (n1 < n2)) 
    _ -> throwErr p "'Less than' operation expects two numbers"

eval (Bop p Gt e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return ( VBool (n1 > n2)) 
    _ -> throwErr p "'Greater than' operation expects two numbers"

eval (Bop p Le e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return ( VBool (n1 <= n2)) 
    _ -> throwErr p "'Less than or equal to' operation expects two numbers"


eval (Bop p Ge e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VNum n1, VNum n2) -> return ( VBool (n1 >= n2)) 
    _ -> throwErr p "'Greater than or equal to' operation expects two numbers"


-- Unary operator (not).
eval (Uop p Not e) = do
  v <- eval e
  case v of
    VBool b -> return (VBool (not b))
    _       -> throwErr p "Not expects a boolean"

-- Pairs.
eval (Pair p e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (VPair v1 v2)

eval (Fst p e) = do
  v <- eval e
  case v of
    VPair v1 _ -> return v1
    _          -> throwErr p "fst expects a pair"

eval (Snd p e) = do
  v <- eval e
  case v of
    VPair _ v2 -> return v2
    _          -> throwErr p "snd expects a pair"

-- Sums.
eval (Inl p t e) = do
  v <- eval e
  return (VInl t v)

eval (Inr p t e) = do
  v <- eval e
  return (VInr t v)

eval (Case p e x e1 y e2) = do
  v <- eval e
  case v of
    VInl _ v1 -> do
      modify (\(env, store, loc) -> (M.insert x v1 env, store, loc))
      eval e1
    VInr _ v2 -> do
      modify (\(env, store, loc) -> (M.insert y v2 env, store, loc))
      eval e2
    _ -> throwErr p "Case expects a sum value"

-- References.
eval (Ref p e) = do
  v <- eval e
  (env, store, loc) <- get
  let newStore = M.insert loc v store
  put (env, newStore, loc + 1)
  return (Memloc loc)

eval (Deref p e) = do
  v <- eval e
  case v of
    Memloc l -> do
      (_, store, _) <- get
      case M.lookup l store of
        Just val -> return val
        Nothing  -> throwErr p ("Invalid memory location: " ++ show l)
    _ -> throwErr p "Deref expects a memory location"

eval (Asgn p e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Memloc l -> do
      (env, store, loc) <- get
      let newStore = M.insert l v2 store
      put (env, newStore, loc)
      return VUnit
    _ -> throwErr p "Assignment expects a memory location on the left-hand side"



-- exoume: VUnit: -, VNum Int VBool Bool



-- Top-level evaluation function. Runs the main evaluation function on an empty
-- state and extracts the value and the store. Note that the store is needed as
-- the value may contain memory locations.
--evalTop :: Exp -> Error (Value,Store)
--evalTop e = do
--  let initialEnv = M.fromList [("v1", VUnit), ("v2", VUnit), ("v3",VUnit)] -- Replace VUnit with a sensible initial Value 
--      initialStore = M.empty
--      initialLoc = 0
--      initState = (initialEnv, M.empty, 0) -- M.empty
--  (val, (_, store, _)) <- runStateT (eval e) initState
--  return (val, store)
--
evalTop :: Exp -> Error (Value, Store)
evalTop e = do
  let initialEnv = M.fromList [("v1", VUnit), ("v2", VUnit), ("v3", VUnit)] -- Default to VUnit
      initialStore = M.empty
      initialLoc = 0
      initialState = (initialEnv, initialStore, initialLoc)
  (val, (_, store, _)) <- runStateT (eval e) initialState
  return (val, store)


evalWithType :: Type -> Exp -> Error (Value, Store)
evalWithType t e = do
  let initialEnv = M.fromList [("v1", defaultValue t), ("v2", defaultValue t), ("v3", defaultValue t)]
      initialStore = M.empty
      initialLoc = 0
      initialState = (initialEnv, initialStore, initialLoc)
  (val, (_, store, _)) <- runStateT (eval e) initialState
  return (val, store)


defaultValue :: Type -> Value
defaultValue TInt = VNum 0
defaultValue TBool = VBool False
defaultValue TUnit = VUnit
defaultValue (TRef t) = Memloc 0
defaultValue _ = VUnit
