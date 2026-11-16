{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.BoundsCheck
--
-- Static index-bounds checking by symbolic evaluation.
--
-- The checker evaluates a program symbolically, inlining functions at every
-- call, and records an obligation at every array access:
--
--   * @index@ and the element reads of @gather@: each index component is in
--     @[0, dim)@;
--   * @scatter@ routes (all forms): each route is in bounds for the
--     destination, under the scatter's guard when it has one (a @permute@
--     target need not be: out-of-range targets are dropped);
--   * @slice@, @zipWith@, @append@, @reshape@: the shapes agree;
--   * @make_index@ and bound annotations: the asserted bound holds.
--
-- Each obligation carries its path condition: the ranges of the enclosing
-- @generate@ binders, the branches taken by enclosing @if@s, and scatter
-- guards.  Integer expressions become exact SMT terms, with truncating
-- division and remainder to match the C backend.
--
-- Nothing is assumed.  The only global facts are definitions of fresh
-- variables (a fresh @v@ equal to a term, or a witness whose range holds
-- whenever its array is non-empty), so every fact is satisfiable for every
-- value of the program's inputs.
--
-- Values the checker cannot compute exactly (file contents, reduction and
-- scatter results, loop state, integers derived from floats) become
-- uninterpreted values keyed by the expression that produced them, so the same
-- expression gives the same value.  A counterexample that involves only exact
-- variables is reported as an error.  One that involves an uninterpreted value
-- may be spurious and is reported as a warning.
module Language.Hydrangea.BoundsCheck
  ( BoundsIssue (..)
  , IssueKind (..)
  , checkProgramBounds
  , checkProgramBoundsWith
  ) where

import Control.Monad (forM, forM_, void, when, zipWithM)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (State, get, gets, modify', put, runState)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.SBV
  ( SBool
  , SInteger
  , constrain
  , literal
  , runSMTWith
  , verbose
  , z3
  , sBool
  , sFalse
  , sInteger
  , sNot
  , sQuot
  , sRem
  , sTrue
  , ite
  , (.&&)
  , (.<)
  , (.<=)
  , (.<=>)
  , (.==)
  , (.||)
  )
import Control.Exception (SomeException, try)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Data.SBV.Control (CheckSatResult (..), checkSat, getValue, query, timeout)
import Language.Hydrangea.Lexer (Range (..))
import Language.Hydrangea.Predicate (RefinePred (..), RefineTerm (..), RelOp (..))
import Language.Hydrangea.Syntax

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

data IssueKind = IssueError | IssueWarning
  deriving (Eq, Show)

-- | An obligation that was refuted ('IssueError') or could not be proved
-- ('IssueWarning').
data BoundsIssue = BoundsIssue
  { biKind   :: IssueKind
  , biRange  :: Range
  , biWhat   :: String
  , biDetail :: String
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Symbolic terms
-- ---------------------------------------------------------------------------

-- | Integer terms.  'TQuot' and 'TRem' truncate toward zero, as in C.
data T
  = TV !Int
  | TC !Integer
  | TAdd T T
  | TSub T T
  | TMul T T
  | TQuot T T
  | TRem T T
  | TIte F T T
  deriving (Eq, Ord, Show)

-- | Boolean formulas.
data F
  = FC !Bool
  | FV !Int
  | FLt T T
  | FLe T T
  | FEq T T
  | FNot F
  | FAnd F F
  | FOr F F
  | FIff F F
  deriving (Eq, Ord, Show)

-- | Float values are never reasoned about.  They are kept as structural terms
-- only so that integers derived from equal float expressions are equal.
data FT
  = FLit Double
  | FUn String [Atom]
  deriving (Eq, Ord, Show)

-- | Keys for uninterpreted values.
data Atom
  = AI T
  | AF FT
  | AB F
  | AS ByteString
  | AN Int
  deriving (Eq, Ord, Show)

type Key = [Atom]

tAdd, tSub, tMul :: T -> T -> T
tAdd (TC a) (TC b) = TC (a + b)
tAdd (TC 0) t = t
tAdd t (TC 0) = t
tAdd a b = TAdd a b
tSub (TC a) (TC b) = TC (a - b)
tSub t (TC 0) = t
-- @a - (a / m) * m@ is the truncating remainder, by definition.
tSub a (TMul (TQuot a' m) m')
  | a == a' && m == m' = TRem a m
tSub a (TMul m' (TQuot a' m))
  | a == a' && m == m' = TRem a m
tSub a b = TSub a b
tMul (TC a) (TC b) = TC (a * b)
tMul (TC 1) t = t
tMul t (TC 1) = t
tMul (TC 0) _ = TC 0
tMul _ (TC 0) = TC 0
tMul a b = TMul a b

tQuot, tRem :: T -> T -> T
tQuot (TC a) (TC b) | b /= 0 = TC (a `quot` b)
tQuot t (TC 1) = t
tQuot a b = TQuot a b
tRem (TC a) (TC b) | b /= 0 = TC (a `rem` b)
tRem a b = TRem a b

tIte :: F -> T -> T -> T
tIte (FC True) a _ = a
tIte (FC False) _ b = b
tIte c a b
  | a == b = a
  | otherwise = TIte c a b

fLt, fLe, fEq :: T -> T -> F
fLt (TC a) (TC b) = FC (a < b)
fLt a b = FLt a b
fLe (TC a) (TC b) = FC (a <= b)
fLe a b
  | a == b = FC True
  | otherwise = FLe a b
fEq (TC a) (TC b) = FC (a == b)
fEq a b
  | a == b = FC True
  | otherwise = FEq a b

fNot :: F -> F
fNot (FC b) = FC (not b)
fNot (FNot f) = f
fNot f = FNot f

fAnd, fOr, fIff :: F -> F -> F
fAnd (FC True) f = f
fAnd f (FC True) = f
fAnd (FC False) _ = FC False
fAnd _ (FC False) = FC False
fAnd a b = FAnd a b
fOr (FC False) f = f
fOr f (FC False) = f
fOr (FC True) _ = FC True
fOr _ (FC True) = FC True
fOr a b = FOr a b
fIff (FC True) f = f
fIff f (FC True) = f
fIff a b
  | a == b = FC True
  | otherwise = FIff a b

fAll :: [F] -> F
fAll = foldr fAnd (FC True)

fIte :: F -> F -> F -> F
fIte c a b = fOr (fAnd c a) (fAnd (fNot c) b)

termVars :: T -> IS.IntSet
termVars t = case t of
  TV v -> IS.singleton v
  TC _ -> IS.empty
  TAdd a b -> termVars a <> termVars b
  TSub a b -> termVars a <> termVars b
  TMul a b -> termVars a <> termVars b
  TQuot a b -> termVars a <> termVars b
  TRem a b -> termVars a <> termVars b
  TIte c a b -> formVars c <> termVars a <> termVars b

formVars :: F -> IS.IntSet
formVars f = case f of
  FC _ -> IS.empty
  FV v -> IS.singleton v
  FLt a b -> termVars a <> termVars b
  FLe a b -> termVars a <> termVars b
  FEq a b -> termVars a <> termVars b
  FNot a -> formVars a
  FAnd a b -> formVars a <> formVars b
  FOr a b -> formVars a <> formVars b
  FIff a b -> formVars a <> formVars b

atomicT :: T -> Bool
atomicT (TV _) = True
atomicT (TC _) = True
atomicT _ = False

-- | Index @ix@ is in bounds for shape @sh@.
inBounds :: [T] -> [T] -> F
inBounds ix sh = fAll [fAnd (fLe (TC 0) i) (fLt i n) | (i, n) <- zip ix sh]

-- ---------------------------------------------------------------------------
-- Symbolic values
-- ---------------------------------------------------------------------------

data V
  = VI T
  | VB F
  | VF FT
  | VUnit
  | VStr ByteString
  | VVec [V]
  | VPair V V
  | VRec (Map Var V)
  | VArr Arr
  | VFun (V -> M V)
  | -- | A value the checker does not know.  It becomes an integer, boolean,
    -- float, pair or array when it is used as one.
    VUnk Key

data Arr = Arr
  { arrShape :: [T]
  , -- | The element at an index.  Callers ensure the index is in bounds
    -- (or record an obligation that it is) before relying on the result.
    arrElem  :: [T] -> M V
  }

-- ---------------------------------------------------------------------------
-- The checking monad
-- ---------------------------------------------------------------------------

data Env = Env
  { envVars    :: Map Var V
  , envPath    :: [F]
  , envCollect :: Bool
  }

data VarInfo = VarInfo
  { viName  :: String
  , viExact :: Bool
  , viBool  :: Bool
  , viShown :: Bool
    -- ^ Shown in counterexamples: program inputs and index variables.
  }

-- | An obligation: its location, what it protects, its path condition and
-- its goal.
data Obl = Obl Range String [F] F
  deriving (Eq, Ord)

data St = St
  { stNext  :: !Int
  , stVars  :: IM.IntMap VarInfo
  , stDefs  :: IM.IntMap [F]
  , stMemoI :: Map Key Int
  , stMemoB :: Map Key Int
  , stObls  :: [Obl]
  }

type M = ReaderT Env (State St)

newVar :: String -> Bool -> Bool -> M Int
newVar name exact isBool = newVarShown name exact isBool False

newVarShown :: String -> Bool -> Bool -> Bool -> M Int
newVarShown name exact isBool shown = do
  st <- get
  let n = stNext st
  put st {stNext = n + 1, stVars = IM.insert n (VarInfo name exact isBool shown) (stVars st)}
  pure n

-- | Give an index variable the name of the pattern that binds it.
nameIndexVar :: String -> T -> M ()
nameIndexVar name t = case t of
  TV n -> modify' $ \st ->
    st {stVars = IM.adjust (\i -> if viShown i && take 1 (viName i) == "$" then i {viName = name} else i) n (stVars st)}
  _ -> pure ()

freshInt :: String -> Bool -> M T
freshInt name exact = TV <$> newVar name exact False

freshBool :: String -> Bool -> M F
freshBool name exact = FV <$> newVar name exact True

-- | Record a fact about the variable @v@.  Facts must be satisfiable for every
-- value of the variables they do not define.
addDef :: Int -> F -> M ()
addDef v f = modify' $ \st -> st {stDefs = IM.insertWith (++) v [f] (stDefs st)}

-- | Name a non-atomic term by a fresh variable, to keep terms small.
shareT :: String -> T -> M T
shareT name t
  | atomicT t = pure t
  | otherwise = do
      v <- newVar name True False
      addDef v (fEq (TV v) t)
      pure (TV v)

shareF :: String -> F -> M F
shareF name f = case f of
  FC _ -> pure f
  FV _ -> pure f
  _ -> do
    v <- newVar name True True
    addDef v (fIff (FV v) f)
    pure (FV v)

-- | An uninterpreted integer, the same for the same key.
memoInt :: Key -> M T
memoInt k = do
  m <- gets stMemoI
  case M.lookup k m of
    Just v -> pure (TV v)
    Nothing -> do
      v <- newVarShown (keyName k) (isInputKey k) False (isInputKey k)
      modify' $ \st -> st {stMemoI = M.insert k v (stMemoI st)}
      pure (TV v)

memoBool :: Key -> M F
memoBool k = do
  m <- gets stMemoB
  case M.lookup k m of
    Just v -> pure (FV v)
    Nothing -> do
      v <- newVar (keyName k) (isInputKey k) True
      modify' $ \st -> st {stMemoB = M.insert k v (stMemoB st)}
      pure (FV v)

keyName :: Key -> String
keyName k = case k of
  (AS "input" : AN i : rest) -> "arg" ++ show i ++ suffix rest
  _ -> case [BS.unpack s | AS s <- k] of
    (s : _) -> s
    [] -> "unknown"
  where
    suffix rest = case rest of
      (AS "dim" : AN d : _) -> ".dim" ++ show d
      (AS "@" : _) -> "[..]"
      _ -> ""

-- | Arguments of an entry point are arbitrary, so values derived from them
-- are exact: a counterexample in terms of them is a real one.
isInputKey :: Key -> Bool
isInputKey (AS "input" : _) = True
isInputKey _ = False

freshKey :: String -> M Key
freshKey tag = do
  n <- newVar tag False False
  pure [AS (BS.pack tag), AN n]

underPath :: F -> M a -> M a
underPath f = local $ \e -> e {envPath = f : envPath e}

noCollect :: M a -> M a
noCollect = local $ \e -> e {envCollect = False}

withVars :: Map Var V -> M a -> M a
withVars vs = local $ \e -> e {envVars = vs}

-- | Record an obligation at the current path, unless it holds trivially.
oblige :: Range -> String -> F -> M ()
oblige r what goal = do
  collect <- asks envCollect
  path <- asks envPath
  when (collect && goal /= FC True && FC False `notElem` path) $
    modify' $ \st -> st {stObls = Obl r what path goal : stObls st}

-- | Record an obligation the checker cannot express.  It is always reported
-- as a warning.
cannotCheck :: Range -> String -> M ()
cannotCheck r what = do
  g <- freshBool "unexpressible" False
  oblige r what g

-- | Evaluate @body@ for every index of @shape@, by evaluating it once at fresh
-- index variables that range over the shape.
forallIdx :: [T] -> ([T] -> M a) -> M a
forallIdx shape body = do
  ks <- forM (zip [0 :: Int ..] shape) $ \(d, _) -> TV <$> newVarShown ("$k" ++ show d) True False True
  underPath (inBounds ks shape) (body ks)

-- ---------------------------------------------------------------------------
-- Coercions
-- ---------------------------------------------------------------------------

asInt :: V -> M T
asInt v = case v of
  VI t -> pure t
  VUnk k -> memoInt k
  VVec [x] -> asInt x
  _ -> freshInt "non-integer" False

asBool :: V -> M F
asBool v = case v of
  VB f -> pure f
  VUnk k -> memoBool k
  _ -> freshBool "non-boolean" False

asFloat :: V -> FT
asFloat v = case v of
  VF f -> f
  VUnk k -> FUn "unknown" k
  VI t -> FUn "of-int" [AI t]
  _ -> FUn "non-float" []

keyOf :: V -> M Key
keyOf v = case v of
  VI t -> pure [AI t]
  VB f -> pure [AB f]
  VF f -> pure [AF f]
  VUnit -> pure [AS "()"]
  VStr s -> pure [AS s]
  VVec vs -> concat <$> mapM keyOf vs
  VPair a b -> (++) <$> keyOf a <*> keyOf b
  VUnk k -> pure k
  _ -> freshKey "opaque"

-- | Index components of an index value, if its rank is known.
ixTerms :: V -> M (Maybe [T])
ixTerms v = case v of
  VUnit -> pure (Just [])
  VI t -> pure (Just [t])
  VVec vs -> Just <$> mapM asInt vs
  VUnk k -> Just . (: []) <$> memoInt k
  _ -> pure Nothing

shapeOf :: V -> M (Maybe [T])
shapeOf = ixTerms

-- | View a value as an array.  An unknown value is an array whose dimensions
-- and elements are uninterpreted; @rank@ gives the number of its dimensions.
resolveArr :: Int -> V -> M (Maybe Arr)
resolveArr rank v = case v of
  VArr a -> pure (Just a)
  VUnk k -> do
    dims <- mapM (\d -> memoInt (k ++ [AS "dim", AN d])) [0 .. rank - 1]
    pure (Just (Arr dims (\ix -> pure (VUnk (k ++ AS "@" : map AI ix)))))
  _ -> pure Nothing

-- | An array of the given shape whose elements are unknown.
unknownElems :: String -> [T] -> M Arr
unknownElems tag sh = do
  k <- freshKey tag
  pure (Arr sh (\ix -> pure (VUnk (k ++ AS "@" : map AI ix))))

-- ---------------------------------------------------------------------------
-- Merging the branches of a conditional
-- ---------------------------------------------------------------------------

merge :: F -> V -> V -> M V
merge (FC True) a _ = pure a
merge (FC False) _ b = pure b
merge c a b = case (a, b) of
  (VI x, VI y) -> pure (VI (tIte c x y))
  (VB x, VB y) -> pure (VB (fIte c x y))
  (VF x, VF y)
    | x == y -> pure a
    | otherwise -> pure (VF (FUn "ite" [AB c, AF x, AF y]))
  (VUnit, _) -> pure VUnit
  (VStr _, _) -> pure a
  (VVec xs, VVec ys)
    | length xs == length ys -> VVec <$> zipWithM (merge c) xs ys
  (VPair a1 a2, VPair b1 b2) -> VPair <$> merge c a1 b1 <*> merge c a2 b2
  (VRec xs, VRec ys) -> VRec <$> sequence (M.intersectionWith (merge c) xs ys)
  (VArr x, VArr y)
    | length (arrShape x) == length (arrShape y) ->
        pure . VArr $
          Arr
            (zipWith (tIte c) (arrShape x) (arrShape y))
            ( \ix -> do
                ex <- underPath c (arrElem x ix)
                ey <- underPath (fNot c) (arrElem y ix)
                merge c ex ey
            )
  (VFun f, VFun g) ->
    pure . VFun $ \v -> do
      x <- underPath c (f v)
      y <- underPath (fNot c) (g v)
      merge c x y
  (VUnk _, VUnk _) -> do
    ka <- keyOf a
    kb <- keyOf b
    pure (VUnk (AS "ite" : AB c : ka ++ AS "|" : kb))
  (VUnk _, _) -> mergeUnk b a (fNot c)
  (_, VUnk _) -> mergeUnk a b c
  _ -> VUnk <$> freshKey "merge"
  where
    -- @known@ is taken when @c'@ holds, the unknown value otherwise.
    mergeUnk known unk c' = case known of
      VI x -> VI . tIte c' x <$> asInt unk
      VB x -> VB . fIte c' x <$> asBool unk
      VF x -> pure (VF (FUn "ite" [AB c', AF x, AF (asFloat unk)]))
      _ -> VUnk <$> freshKey "merge"

-- ---------------------------------------------------------------------------
-- Patterns and declarations
-- ---------------------------------------------------------------------------

bindPat :: Pat Range -> V -> Map Var V -> M (Map Var V)
bindPat p v env = case p of
  PVar _ x -> do
    case v of
      VI t -> nameIndexVar (BS.unpack x) t
      _ -> pure ()
    v' <- shareV (BS.unpack x) v
    pure (M.insert x v' env)
  PBound r x be -> do
    t <- asInt v
    b <- asInt =<< withVars env (eval be)
    oblige r ("bound annotation on " ++ BS.unpack x) (fAnd (fLe (TC 0) t) (fLt t b))
    pure (M.insert x (VI t) env)
  PVec _ ps -> case v of
    VVec vs
      | length vs == length ps -> bindAll ps vs env
    VI _
      | [p'] <- ps -> bindPat p' v env
    VUnk k -> bindAll ps [VUnk (k ++ [AS "#", AN i]) | i <- [0 .. length ps - 1]] env
    _ -> bindAll ps [VUnk [AS "pattern", AN i] | i <- [0 .. length ps - 1]] env
  PPair _ p1 p2 -> case v of
    VPair a b -> bindPat p1 a env >>= bindPat p2 b
    VUnk k -> bindPat p1 (VUnk (k ++ [AS "fst"])) env >>= bindPat p2 (VUnk (k ++ [AS "snd"]))
    _ -> bindPat p1 (VUnk [AS "pattern"]) env >>= bindPat p2 (VUnk [AS "pattern"])
  where
    bindAll ps vs e = foldlM' (\acc (p', v') -> bindPat p' v' acc) e (zip ps vs)

foldlM' :: (b -> a -> M b) -> b -> [a] -> M b
foldlM' _ z [] = pure z
foldlM' f z (x : xs) = f z x >>= \z' -> foldlM' f z' xs

-- | Name integer and boolean values by variables, so that counterexamples are
-- readable and terms stay small.
shareV :: String -> V -> M V
shareV name v = case v of
  VI t -> VI <$> shareT name t
  VB f -> VB <$> shareF name f
  _ -> pure v

-- | Bind a declaration in an environment.
bindDec :: Map Var V -> Dec Range -> M (Map Var V)
bindDec env (Dec r name pats mwhere _ body) = case pats of
  [] -> do
    v <- withVars env (eval body)
    v' <- shareV (BS.unpack name) v
    pure (M.insert name v' env)
  _ -> pure (M.insert name (mkFun env pats []) env)
  where
    mkFun cenv ps args = case ps of
      [] -> error "mkFun: no parameters"
      [p] -> VFun $ \a -> callBody cenv (reverse ((p, a) : args))
      (p : rest) -> VFun $ \a -> pure (mkFun cenv rest ((p, a) : args))
    callBody cenv bound = do
      fenv <- foldlM' (\acc (p, a) -> bindPat p a acc) cenv bound
      forM_ mwhere $ \wp -> withVars fenv (checkWhere r name wp)
      withVars fenv (eval body)

-- | Check a @where@ precondition at a call site.
checkWhere :: Range -> Var -> RefinePred -> M ()
checkWhere r fname wp = case wp of
  RPAnd a b -> checkWhere r fname a >> checkWhere r fname b
  RPBound x e -> do
    t <- asInt =<< lookupVar x
    b <- refT e
    oblige r what (fAnd (fLe (TC 0) t) (fLt t b))
  RPRel op (RTElem a) rhs
    | op `elem` [RLe, RLt] -> elemBound a op rhs
  RPRel op lhs (RTElem a)
    | op `elem` [RGe, RGt] -> elemBound a (flipOp op) lhs
  RPRel op lhs rhs -> do
    l <- refT lhs
    rt <- refT rhs
    oblige r what (relF op l rt)
  where
    what = "precondition of " ++ BS.unpack fname
    flipOp RGe = RLe
    flipOp RGt = RLt
    flipOp o = o
    -- @elem a@ is the exclusive upper bound of the elements of @a@.
    elemBound a op rhs = do
      av <- lookupVar a
      ma <- resolveArr 1 av
      bt <- refT rhs
      case ma of
        Nothing -> cannotCheck r what
        Just arr -> forallIdx (arrShape arr) $ \ks -> do
          x <- asInt =<< noCollect (arrElem arr ks)
          oblige r what (relF op (tAdd x (TC 1)) bt)
    refT t = case t of
      RTVar x -> asInt =<< lookupVar x
      RTConst n -> pure (TC n)
      RTDim a i -> do
        av <- lookupVar a
        ma <- resolveArr (i + 1) av
        case ma of
          Just arr
            | i < length (arrShape arr) -> pure (arrShape arr !! i)
          _ -> freshInt "dim" False
      RTElem _ -> freshInt "elem" False
      RTAdd a b -> tAdd <$> refT a <*> refT b
      RTSub a b -> tSub <$> refT a <*> refT b
      RTMul k a -> tMul (TC k) <$> refT a

relF :: RelOp -> T -> T -> F
relF op a b = case op of
  RLt -> fLt a b
  RLe -> fLe a b
  RGt -> fLt b a
  RGe -> fLe b a
  REq -> fEq a b
  RNeq -> fNot (fEq a b)

lookupVar :: Var -> M V
lookupVar x = do
  vs <- asks envVars
  pure (fromMaybe (VUnk [AS "free", AS x]) (M.lookup x vs))

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

apply :: V -> V -> M V
apply f x = case f of
  VFun g -> g x
  _ -> do
    kf <- keyOf f
    kx <- keyOf x
    pure (VUnk (kf ++ AS "$" : kx))

apply2 :: V -> V -> V -> M V
apply2 f x y = apply f x >>= \g -> apply g y

eval :: Exp Range -> M V
eval expr = case expr of
  EInt _ n -> pure (VI (TC n))
  EFloat _ d -> pure (VF (FLit d))
  EVar _ x -> lookupVar x
  EString _ s -> pure (VStr s)
  EUnit _ -> pure VUnit
  EBool _ b -> pure (VB (FC b))
  EVec _ es -> VVec <$> mapM eval es
  EApp _ f x -> do
    fv <- eval f
    xv <- eval x
    apply fv xv
  EIfThen _ c t -> do
    cb <- asBool =<< eval c
    void (underPath cb (eval t))
    pure VUnit
  EIfThenElse _ c t e -> do
    cb <- asBool =<< eval c
    tv <- underPath cb (eval t)
    ev <- underPath (fNot cb) (eval e)
    merge cb tv ev
  ENeg _ e -> do
    v <- eval e
    case v of
      VF f -> pure (VF (FUn "neg" [AF f]))
      _ -> VI . tSub (TC 0) <$> asInt v
  EBinOp _ a op b -> do
    av <- eval a
    bv <- eval b
    binop op av bv
  EUnOp _ op e -> eval e >>= unop op
  EOp _ op -> pure (VFun $ \a -> pure (VFun $ \b -> binop op a b))
  ELetIn _ dec body -> do
    vs <- asks envVars
    vs' <- bindDec vs dec
    withVars vs' (eval body)
  EProj _ i e -> do
    v <- eval e
    pure $ case v of
      VVec vs
        | fromIntegral i < length vs -> vs !! fromIntegral i
      VPair a b
        | i == 0 -> a
        | i == 1 -> b
      VUnk k -> VUnk (k ++ [AS "#", AN (fromIntegral i)])
      _ -> VUnk [AS "proj", AN (fromIntegral i)]
  EPair _ a b -> VPair <$> eval a <*> eval b
  ERecord _ fs -> VRec . M.fromList <$> mapM (\(f, e) -> (f,) <$> eval e) fs
  ERecordProj _ e f -> do
    v <- eval e
    pure $ case v of
      VRec m
        | Just x <- M.lookup f m -> x
      VUnk k -> VUnk (k ++ [AS ".", AS f])
      _ -> VUnk [AS "field", AS f]
  EGenerate _ shE fE -> do
    msh <- shapeOf =<< eval shE
    f <- eval fE
    case msh of
      Nothing -> do
        void (apply f (VUnk [AS "index"]))
        VUnk <$> freshKey "generate"
      Just sh -> do
        forallIdx sh $ \ks -> void (apply f (ixV ks))
        pure (VArr (Arr sh (\ix -> noCollect (apply f (ixV ix)))))
  EMap r fE aE -> do
    f <- eval fE
    av <- eval aE
    mapLike r f [av]
  EZipWith r fE aE bE -> do
    f <- eval fE
    av <- eval aE
    bv <- eval bE
    mapLike r f [av, bv]
  EAppend r aE bE -> do
    av <- eval aE
    bv <- eval bE
    ma <- arrOf av
    mb <- arrOf bv
    case (ma, mb) of
      (Just a, Just b)
        | not (null (arrShape a)) && length (arrShape a) == length (arrShape b) -> do
            let lead = init (arrShape a)
                n1 = last (arrShape a)
                n2 = last (arrShape b)
            oblige r "append: leading dimensions agree" (fAll (zipWith fEq lead (init (arrShape b))))
            pure . VArr $
              Arr
                (lead ++ [tAdd n1 n2])
                ( \ix -> do
                    let k = last ix
                        c = fLt k n1
                    x <- underPath c (arrElem a ix)
                    y <- underPath (fNot c) (arrElem b (init ix ++ [tSub k n1]))
                    merge c x y
                )
      _ -> VUnk <$> freshKey "append"
  EReduce _ fE zE aE -> do
    f <- eval fE
    z <- eval zE
    av <- eval aE
    ma <- arrOf av
    case ma of
      Just a
        | not (null (arrShape a)) -> do
            checkFold f z (arrShape a) (arrElem a)
            VArr <$> unknownElems "reduce" (init (arrShape a))
      _ -> VUnk <$> freshKey "reduce"
  EReduceGenerate _ fE zE shE gE -> do
    f <- eval fE
    z <- eval zE
    msh <- shapeOf =<< eval shE
    g <- eval gE
    case msh of
      Just sh
        | not (null sh) -> do
            -- The generator's own accesses, at every index of the shape.
            forallIdx sh $ \ks -> void (apply g (ixV ks))
            checkFold f z sh (\ix -> apply g (ixV ix))
            VArr <$> unknownElems "reduce" (init sh)
      _ -> do
        void (apply g (VUnk [AS "index"]))
        VUnk <$> freshKey "reduce"
  EFoldl _ fE zE aE -> do
    f <- eval fE
    z <- eval zE
    av <- eval aE
    ma <- arrOf av
    forM_ ma $ \a -> checkFold f z (arrShape a) (arrElem a)
    VUnk <$> freshKey "foldl"
  EFoldlWhile _ pE fE zE aE -> do
    p <- eval pE
    f <- eval fE
    z <- eval zE
    av <- eval aE
    acc <- VUnk <$> freshKey "acc"
    void (apply p acc)
    ma <- arrOf av
    forM_ ma $ \a -> checkFold f z (arrShape a) (arrElem a)
    VUnk <$> freshKey "foldl"
  EScan _ fE zE aE -> scanLike fE zE aE
  EScanInclusive _ fE zE aE -> scanLike fE zE aE
  EScanR _ fE zE aE -> scanLike fE zE aE
  EScanRInclusive _ fE zE aE -> scanLike fE zE aE
  ESegmentedReduce r fE zE oE vE -> do
    f <- eval fE
    z <- eval zE
    ov <- eval oE
    vv <- eval vE
    mo <- arrOf ov
    mv <- arrOf vv
    case (mo, mv) of
      (Just o, Just v) -> do
        checkFold f z (arrShape v) (arrElem v)
        let nsegs = tSub (headOr (TC 0) (arrShape o)) (TC 1)
            nvals = headOr (TC 0) (arrShape v)
        forallIdx [nsegs] $ \ks -> do
          let i = headOr (TC 0) ks
          lo <- asInt =<< noCollect (arrElem o [i])
          hi <- asInt =<< noCollect (arrElem o [tAdd i (TC 1)])
          oblige r "segmented_reduce: segment is in bounds"
            (fAll [fLe (TC 0) lo, fLe lo hi, fLe hi nvals])
        VArr <$> unknownElems "segmented" [nsegs]
      _ -> VUnk <$> freshKey "segmented"
  ESortIndices _ aE -> do
    av <- eval aE
    ma <- arrOf av
    case ma of
      Just a
        | [n] <- arrShape a -> VArr <$> witnessArr "sort_indices" n
      _ -> VUnk <$> freshKey "sort_indices"
  EIota _ nE -> do
    n <- asInt =<< eval nE
    pure (VArr (Arr [n] (\ix -> pure (VI (headOr (TC 0) ix)))))
  EMakeIndex r nE aE -> do
    n <- asInt =<< eval nE
    av <- eval aE
    ma <- arrOf av
    case ma of
      Just a -> do
        forallIdx (arrShape a) $ \ks -> do
          x <- asInt =<< noCollect (arrElem a ks)
          oblige r "make_index: element is a valid index" (inBounds [x] [n])
        pure av
      Nothing -> do
        cannotCheck r "make_index: element is a valid index"
        pure av
  ECOOSumDuplicates r nrE ncE nnzE rowsE colsE valsE -> sparse r False nrE ncE nnzE rowsE colsE valsE
  ECSRFromSortedCOO r nrE ncE nnzE rowsE colsE valsE -> sparse r True nrE ncE nnzE rowsE colsE valsE
  EPermute _ cE dE pE aE -> do
    c <- eval cE
    dv <- eval dE
    p <- eval pE
    av <- eval aE
    md <- arrOf dv
    ma <- arrOf av
    case (md, ma) of
      (Just d, Just a) -> do
        -- Targets outside the destination are dropped, so they need no
        -- obligation; the permutation function and combine are still checked.
        forallIdx (arrShape a) $ \ks -> do
          void (apply p (ixV ks))
          x <- noCollect (arrElem a ks)
          acc <- VUnk <$> freshKey "acc"
          void (apply2 c acc x)
        VArr <$> unknownElems "permute" (arrShape d)
      _ -> VUnk <$> freshKey "permute"
  EScatter r cE dE iE vE -> do
    c <- eval cE
    dv <- eval dE
    iv <- eval iE
    vv <- eval vE
    scatterPhases r c dv [(iv, Just vv, Nothing)]
  EScatterGuarded r cE dE iE vE gE -> do
    c <- eval cE
    dv <- eval dE
    iv <- eval iE
    vv <- eval vE
    gv <- eval gE
    scatterPhases r c dv [(iv, Just vv, Just gv)]
  EScatterGenerate r cE dE iE fE -> do
    c <- eval cE
    dv <- eval dE
    iv <- eval iE
    f <- eval fE
    mi <- arrOf iv
    vv <- case mi of
      Just i -> do
        forallIdx (arrShape i) $ \ks -> void (apply f (ixV ks))
        pure (VArr (Arr (arrShape i) (\ix -> noCollect (apply f (ixV ix)))))
      Nothing -> do
        void (apply f (VUnk [AS "index"]))
        VUnk <$> freshKey "values"
    scatterPhases r c dv [(iv, Just vv, Nothing)]
  EScatterChain r cE dE phases -> do
    c <- eval cE
    dv <- eval dE
    ps <- forM phases $ \(ScatterPhase iE vE mgE) -> do
      iv <- eval iE
      vv <- eval vE
      gv <- traverse eval mgE
      pure (iv, Just vv, gv)
    scatterPhases r c dv ps
  EScatterGen r cE dE phases -> do
    c <- eval cE
    dv <- eval dE
    ps <- forM phases $ \(ScatterGenPhase shE iF vF mgF) -> do
      msh <- shapeOf =<< eval shE
      fi <- eval iF
      fv <- eval vF
      fg <- traverse eval mgF
      case msh of
        Nothing -> do
          u <- VUnk <$> freshKey "phase"
          pure (u, Nothing, Nothing)
        Just sh -> do
          let gen f = VArr (Arr sh (\ix -> noCollect (apply f (ixV ix))))
          -- Check the generator bodies themselves once over the phase shape.
          forallIdx sh $ \ks -> do
            g <- case fg of
              Nothing -> pure (FC True)
              Just f -> asBool =<< apply f (ixV ks)
            underPath g $ void (apply fi (ixV ks) >> apply fv (ixV ks))
          pure (gen fi, Just (gen fv), gen <$> fg)
    scatterPhases r c dv ps
  EGather r iE aE -> do
    iv <- eval iE
    av <- eval aE
    mi <- arrOf iv
    case mi of
      Just i -> do
        forallIdx (arrShape i) $ \ks -> do
          ix <- noCollect (arrElem i ks)
          void (readChecked r "gather" av ix)
        pure (VArr (Arr (arrShape i) (\ks -> noCollect (arrElem i ks >>= readChecked r "gather" av))))
      Nothing -> do
        cannotCheck r "gather"
        VUnk <$> freshKey "gather"
  EIndex r ixE aE -> do
    ixv <- eval ixE
    av <- eval aE
    readChecked r "index" av ixv
  ECheckIndex _ ixE dE aE -> do
    ixv <- eval ixE
    dflt <- eval dE
    av <- eval aE
    mix <- ixTerms ixv
    ma <- arrOf' (maybe 1 length mix) av
    case (mix, ma) of
      (Just ix, Just a)
        | length ix == length (arrShape a) -> do
            let c = inBounds ix (arrShape a)
            x <- underPath c (noCollect (arrElem a ix))
            merge c x dflt
      _ -> VUnk <$> freshKey "check_index"
  ECompact r _ zE gE xE -> do
    z <- eval zE
    gv <- eval gE
    xv <- eval xE
    mg <- arrOf gv
    mx <- arrOf xv
    case (mg, mx) of
      (Just g, Just x)
        | [n] <- arrShape x -> do
            oblige r "compact: guard and source have the same length" (fAll (zipWith fEq (arrShape g) [n]))
            k <- freshKey "compact"
            pure . VArr . Arr [n] $ \ix -> do
              let p = headOr (TC 0) ix
              picked <- memoBool (k ++ [AS "kept", AI p])
              j <- witness (k ++ [AS "from", AI p]) n
              y <- underPath picked (noCollect (arrElem x [j]))
              merge picked y z
      _ -> VUnk <$> freshKey "compact"
  EFill _ shE vE -> do
    msh <- shapeOf =<< eval shE
    v <- eval vE
    case msh of
      Just sh -> pure (VArr (Arr sh (const (pure v))))
      Nothing -> VUnk <$> freshKey "fill"
  EShapeOf _ aE -> do
    av <- eval aE
    ma <- arrOf av
    case ma of
      Just a -> pure (VVec (map VI (arrShape a)))
      Nothing -> VUnk <$> freshKey "shape"
  EReplicate _ dims aE -> do
    av <- eval aE
    ma <- arrOf av
    case ma of
      Nothing -> VUnk <$> freshKey "replicate"
      Just a -> do
        -- For each output axis: Left = a new axis, Right i = source axis i.
        (axes, _) <-
          foldlM'
            ( \(acc, i) d -> case d of
                ShapeAll _ -> pure (acc ++ [Right i], i + 1)
                ShapeAny _ e -> do
                  t <- asInt =<< eval e
                  pure (acc ++ [Left t], i)
                ShapeDim _ e -> do
                  t <- asInt =<< eval e
                  pure (acc ++ [Left t], i)
            )
            ([], 0 :: Int)
            dims
        let sh = [either id (\i -> idx (arrShape a) i) ax | ax <- axes]
            srcIx ix = [t | (Right _, t) <- zip axes ix]
        pure (VArr (Arr sh (\ix -> arrElem a (srcIx ix))))
  ESlice r dims aE -> do
    av <- eval aE
    ma <- arrOf av
    case ma of
      Just a
        | length dims == length (arrShape a) -> do
            parts <- forM (zip dims (arrShape a)) $ \(d, n) -> case d of
              SliceAll _ -> pure (TC 0, n)
              SliceRange _ sE lE -> do
                s <- asInt =<< eval sE
                l <- asInt =<< eval lE
                oblige r "slice is in bounds" (fAll [fLe (TC 0) s, fLe (TC 0) l, fLe (tAdd s l) n])
                pure (s, l)
            pure (VArr (Arr (map snd parts) (\ix -> arrElem a (zipWith tAdd (map fst parts) ix))))
      _ -> VUnk <$> freshKey "slice"
  EReshape r shE aE -> do
    msh <- shapeOf =<< eval shE
    av <- eval aE
    ma <- arrOf av
    case (msh, ma) of
      (Just sh, Just a) -> do
        oblige r "reshape preserves the number of elements" (fEq (product' sh) (product' (arrShape a)))
        pure (VArr (Arr sh (\ix -> arrElem a (delin (arrShape a) (lin sh ix)))))
      _ -> VUnk <$> freshKey "reshape"
  EReadArray _ shE fE -> readArr shE fE
  EReadArrayFloat _ shE fE -> readArr shE fE
  EWriteArray _ aE fE -> eval aE >> eval fE >> pure VUnit
  EWriteArrayFloat _ aE fE -> eval aE >> eval fE >> pure VUnit
  EGetEnvInt _ nE -> do
    nv <- eval nE
    let name = case nv of
          VStr s -> filter (/= '"') (BS.unpack s)
          _ -> "env"
    k <- keyOf nv
    -- The same variable read twice gives the same value.
    m <- gets stMemoI
    case M.lookup (AS "env" : k) m of
      Just v -> pure (VI (TV v))
      Nothing -> do
        v <- newVarShown name True False True
        modify' $ \st -> st {stMemoI = M.insert (AS "env" : k) v (stMemoI st)}
        pure (VI (TV v))
  EGetEnvString _ nE -> eval nE >> pure (VStr "")
  EStencil _ bc accE aE -> do
    acc <- eval accE
    av <- eval aE
    case bc of
      BConst e -> void (eval e)
      _ -> pure ()
    ma <- arrOf av
    case ma of
      Just a -> do
        let rank = length (arrShape a)
        k <- freshKey "stencil"
        let accessor ix = curried rank [] $ \offs -> do
              key <- concat <$> mapM keyOf offs
              pure (VUnk (k ++ map AI ix ++ AS "+" : key))
        forallIdx (arrShape a) $ \ks -> void (apply acc (accessor ks))
        pure (VArr (Arr (arrShape a) (\ix -> noCollect (apply acc (accessor ix)))))
      Nothing -> VUnk <$> freshKey "stencil"
  EIterate _ nE initE stepE -> do
    void (eval nE)
    s0 <- eval initE
    f <- eval stepE
    iterateLoop s0 f
  EBoundLetIn r x bE rhsE body -> do
    t <- asInt =<< eval rhsE
    b <- asInt =<< eval bE
    oblige r ("bound annotation on " ++ BS.unpack x) (fAnd (fLe (TC 0) t) (fLt t b))
    t' <- shareT (BS.unpack x) t
    vs <- asks envVars
    withVars (M.insert x (VI t') vs) (eval body)
  EReify _ e -> eval e
  where
    ixV ks = VVec (map VI ks)
    headOr d xs = case xs of
      (x : _) -> x
      [] -> d
    idx xs i = if i < length xs then xs !! i else TC 0
    curried :: Int -> [V] -> ([V] -> M V) -> V
    curried 0 acc k = VFun (\_ -> k (reverse acc))
    curried 1 acc k = VFun (\v -> k (reverse (v : acc)))
    curried n acc k = VFun (\v -> pure (curried (n - 1) (v : acc) k))
    readArr shE fE = do
      msh <- shapeOf =<< eval shE
      void (eval fE)
      case msh of
        Just sh -> VArr <$> unknownElems "file" sh
        Nothing -> VUnk <$> freshKey "file"
    scanLike fE zE aE = do
      f <- eval fE
      z <- eval zE
      av <- eval aE
      ma <- arrOf av
      case ma of
        Just a -> do
          checkFold f z (arrShape a) (arrElem a)
          VArr <$> unknownElems "scan" (arrShape a)
        Nothing -> VUnk <$> freshKey "scan"
    -- The sparse-matrix builtins index by their row and column inputs, so
    -- those must be in bounds.  Their outputs keep every input entry.
    sparse r csr nrE ncE nnzE rowsE colsE valsE = do
      nr <- asInt =<< eval nrE
      nc <- asInt =<< eval ncE
      nnz <- eval nnzE
      rowsV <- eval rowsE
      colsV <- eval colsE
      valsV <- eval valsE
      mrows <- arrOf rowsV
      mcols <- arrOf colsV
      let name = if csr then "csr_from_sorted_coo" else "coo_sum_duplicates"
      case (mrows, mcols) of
        (Just rows, Just cols) -> do
          forM_ [(rows, nr, "row"), (cols, nc, "column")] $ \(a, bound, what) ->
            forallIdx (arrShape a) $ \ks -> do
              x <- asInt =<< noCollect (arrElem a ks)
              oblige r (name ++ ": " ++ what ++ " is in bounds") (inBounds [x] [bound])
          let n = case arrShape rows of
                (d : _) -> d
                [] -> TC 0
          rows' <- drawnFrom "rows" n rows
          cols' <- drawnFrom "cols" n cols
          vals' <- unknownElems "vals" [n]
          k <- freshKey "row_ptr"
          let rowPtr = Arr [tAdd nr (TC 1)] $ \ix ->
                VI <$> witnessOf False (k ++ map AI ix) (tAdd n (TC 1))
              common = [("nrows", VI nr), ("ncols", VI nc), ("nnz", nnz), ("vals", VArr vals')]
          pure . VRec . M.fromList $
            if csr
              then common ++ [("row_ptr", VArr rowPtr), ("col_idx", VArr cols')]
              else common ++ [("rows", VArr rows'), ("cols", VArr cols')]
        _ -> do
          void (pure valsV)
          cannotCheck r (name ++ " inputs")
          VUnk <$> freshKey name

arrOf :: V -> M (Maybe Arr)
arrOf = arrOf' 1

arrOf' :: Int -> V -> M (Maybe Arr)
arrOf' = resolveArr

-- | Read an element, recording the obligation that the index is in bounds.
readChecked :: Range -> String -> V -> V -> M V
readChecked r what av ixv = do
  mix <- ixTerms ixv
  case mix of
    Nothing -> do
      cannotCheck r what
      VUnk <$> freshKey what
    Just ix -> do
      ma <- arrOf' (length ix) av
      case ma of
        Just a
          | length (arrShape a) == length ix -> do
              oblige r what (inBounds ix (arrShape a))
              underPath (inBounds ix (arrShape a)) (noCollect (arrElem a ix))
        _ -> do
          cannotCheck r what
          VUnk <$> freshKey what

-- | Arrays for @map@ and @zipWith@: the inputs must have the same shape.
mapLike :: Range -> V -> [V] -> M V
mapLike r f vs = do
  mas <- mapM arrOf vs
  case sequence mas of
    Just as@(a : rest) -> do
      forM_ rest $ \b ->
        if length (arrShape b) == length (arrShape a)
          then oblige r "arrays have the same shape" (fAll (zipWith fEq (arrShape a) (arrShape b)))
          else cannotCheck r "arrays have the same shape"
      let elemsAt ix = mapM (\b -> arrElem b ix) as
      forallIdx (arrShape a) $ \ks -> do
        xs <- noCollect (elemsAt ks)
        void (applyN f xs)
      pure (VArr (Arr (arrShape a) (\ix -> noCollect (elemsAt ix >>= applyN f))))
    _ -> do
      cannotCheck r "map over a value that is not an array"
      VUnk <$> freshKey "map"
  where
    applyN g xs = case xs of
      [] -> pure g
      (x : rest) -> apply g x >>= \g' -> applyN g' rest

-- | Check the obligations inside a fold's combining function, with an unknown
-- accumulator and an arbitrary element.
checkFold :: V -> V -> [T] -> ([T] -> M V) -> M ()
checkFold f _ sh el =
  forallIdx sh $ \ks -> do
    x <- noCollect (el ks)
    acc <- VUnk <$> freshKey "acc"
    void (apply2 f acc x)

-- | The obligations of a (possibly multi-phase, possibly guarded) scatter.
scatterPhases :: Range -> V -> V -> [(V, Maybe V, Maybe V)] -> M V
scatterPhases r c dv phases = do
  md <- arrOf dv
  case md of
    Nothing -> do
      cannotCheck r "scatter destination"
      VUnk <$> freshKey "scatter"
    Just d -> do
      let dsh = arrShape d
      forM_ phases $ \(iv, mvv, mgv) -> do
        mi <- arrOf iv
        case mi of
          Nothing -> cannotCheck r "scatter route"
          Just i -> do
            mv <- traverse arrOf mvv
            mg <- traverse arrOf mgv
            forM_ mv $ \mva -> forM_ mva $ \va ->
              sameShape "scatter: routes and values have the same shape" (arrShape i) (arrShape va)
            forM_ mg $ \mga -> forM_ mga $ \ga ->
              sameShape "scatter: routes and guards have the same shape" (arrShape i) (arrShape ga)
            forallIdx (arrShape i) $ \ks -> do
              g <- case mg of
                Just (Just ga) -> asBool =<< noCollect (arrElem ga ks)
                Just Nothing -> freshBool "guard" False
                Nothing -> pure (FC True)
              underPath g $ do
                route <- noCollect (arrElem i ks)
                mix <- ixTerms route
                case mix of
                  Just ix
                    | length ix == length dsh ->
                        oblige r "scatter route" (inBounds ix dsh)
                  _ -> cannotCheck r "scatter route"
                case mv of
                  Just (Just va) -> do
                    x <- noCollect (arrElem va ks)
                    acc <- VUnk <$> freshKey "acc"
                    void (apply2 c acc x)
                  _ -> pure ()
      VArr <$> unknownElems "scatter" dsh
  where
    sameShape what a b
      | length a == length b = oblige r what (fAll (zipWith fEq a b))
      | otherwise = cannotCheck r what

-- | A permutation of @[0, n)@: every value in the range occurs.
witnessArr :: String -> T -> M Arr
witnessArr tag n = do
  k <- freshKey tag
  pure (Arr [n] (\ix -> VI <$> witnessOf True (k ++ map AI ix) n))

-- | An uninterpreted integer in @[0, n)@ whenever @n > 0@, standing for the
-- source position of some element.  The position is arbitrary only when
-- every source position is used, as in a permutation; otherwise ('witness')
-- a counterexample that relies on it may be spurious.
witness :: Key -> T -> M T
witness = witnessOf False

witnessOf :: Bool -> Key -> T -> M T
witnessOf exact k n = do
  m <- gets stMemoI
  j <- case M.lookup k m of
    Just v -> pure (TV v)
    Nothing -> do
      v <- newVar (keyName k) exact False
      modify' $ \st -> st {stMemoI = M.insert k v (stMemoI st)}
      addDef v (fOr (fLe n (TC 0)) (fAnd (fLe (TC 0) (TV v)) (fLt (TV v) n)))
      pure (TV v)
  pure j

-- | An array of length @n@ whose elements are drawn from @src@, using every
-- position of @src@.
drawnFrom :: String -> T -> Arr -> M Arr
drawnFrom tag n src = do
  k <- freshKey tag
  pure . Arr [n] $ \ix -> do
    let m = case arrShape src of
          (d : _) -> d
          [] -> TC 0
    j <- witnessOf True (k ++ map AI ix) m
    noCollect (arrElem src [j])

product' :: [T] -> T
product' = foldr tMul (TC 1)

-- | Row-major linearisation and its inverse.
lin :: [T] -> [T] -> T
lin sh ix = foldl (\acc (n, i) -> tAdd (tMul acc n) i) (TC 0) (zip sh ix)

delin :: [T] -> T -> [T]
delin sh t = snd (foldr step (t, []) sh)
  where
    step n (rest, acc) = (tQuot rest n, tRem rest n : acc)

-- | Evaluate a loop: the step function is checked against a state whose
-- contents are unknown.  Array shapes are kept when the step preserves them,
-- and otherwise are unknown too.
iterateLoop :: V -> V -> M V
iterateLoop s0 f = do
  saved <- get
  h1 <- havoc True s0
  out <- apply f h1
  if sameShapes h1 out
    then havoc True s0
    else do
      put saved
      h2 <- havoc False s0
      void (apply f h2)
      havoc False s0
  where
    sameShapes a b = case (a, b) of
      (VArr x, VArr y) -> arrShape x == arrShape y
      (VPair a1 a2, VPair b1 b2) -> sameShapes a1 b1 && sameShapes a2 b2
      (VVec xs, VVec ys) -> length xs == length ys && and (zipWith sameShapes xs ys)
      (VRec xs, VRec ys) -> M.keys xs == M.keys ys && and (M.elems (M.intersectionWith sameShapes xs ys))
      (VArr _, _) -> False
      (_, VArr _) -> False
      _ -> True

-- | Replace the contents of a loop state by unknown values.
havoc :: Bool -> V -> M V
havoc keepShapes v = case v of
  VI _ -> VI <$> freshInt "loop" False
  VB _ -> VB <$> freshBool "loop" False
  VF _ -> VF . FUn "loop" <$> freshKey "loop"
  VVec vs -> VVec <$> mapM (havoc keepShapes) vs
  VPair a b -> VPair <$> havoc keepShapes a <*> havoc keepShapes b
  VRec m -> VRec <$> mapM (havoc keepShapes) m
  VArr a -> do
    sh <-
      if keepShapes
        then pure (arrShape a)
        else forM (arrShape a) $ \_ -> do
          d <- freshInt "loop-dim" False
          case d of
            TV x -> addDef x (fLe (TC 0) d)
            _ -> pure ()
          pure d
    VArr <$> unknownElems "loop" sh
  VUnk _ -> VUnk <$> freshKey "loop"
  _ -> pure v

binop :: Operator Range -> V -> V -> M V
binop op a b = case op of
  Plus _ -> int2 tAdd
  Minus _ -> int2 tSub
  Times _ -> int2 tMul
  Divide _ -> int2 tQuot
  Mod _ -> int2 tRem
  Eq _ -> eqLike False
  Neq _ -> eqLike True
  Lt _ -> cmp fLt "<"
  Le _ -> cmp fLe "<="
  Gt _ -> cmp (flip fLt) ">"
  Ge _ -> cmp (flip fLe) ">="
  And _ -> VB <$> (fAnd <$> asBool a <*> asBool b)
  Or _ -> VB <$> (fOr <$> asBool a <*> asBool b)
  PlusF _ -> flt "+."
  MinusF _ -> flt "-."
  TimesF _ -> flt "*."
  DivideF _ -> flt "/."
  EqF _ -> fcmp "=."
  NeqF _ -> VB . fNot <$> memoBool [AS "=.", AF (asFloat a), AF (asFloat b)]
  LtF _ -> fcmp "<."
  LeF _ -> fcmp "<=."
  GtF _ -> fcmp ">."
  GeF _ -> fcmp ">=."
  where
    int2 g = VI <$> (g <$> asInt a <*> asInt b)
    flt name = pure (VF (FUn name [AF (asFloat a), AF (asFloat b)]))
    fcmp name = VB <$> memoBool [AS name, AF (asFloat a), AF (asFloat b)]
    isFloat v = case v of
      VF _ -> True
      _ -> False
    cmp g name
      | isFloat a || isFloat b = fcmp name'
      | otherwise = VB <$> (g <$> asInt a <*> asInt b)
      where
        name' = BS.pack (name ++ ".")
    eqLike negated = do
      r <- case (a, b) of
        (VB x, VB y) -> pure (fIff x y)
        (VB x, VUnk _) -> fIff x <$> asBool b
        (VUnk _, VB y) -> (`fIff` y) <$> asBool a
        _
          | isFloat a || isFloat b -> memoBool [AS "=.", AF (asFloat a), AF (asFloat b)]
          | otherwise -> fEq <$> asInt a <*> asInt b
      pure (VB (if negated then fNot r else r))

unop :: UnOperator Range -> V -> M V
unop op v = case op of
  Not _ -> VB . fNot <$> asBool v
  Fst _ -> pure (proj True)
  Snd _ -> pure (proj False)
  FloatOf _ -> do
    t <- asInt v
    pure (VF (FUn "float_of" [AI t]))
  IntOf _ -> VI <$> memoInt [AS "int_of", AF (asFloat v)]
  Sqrt _ -> fl "sqrt"
  ExpF _ -> fl "exp"
  Log _ -> fl "log"
  Sin _ -> fl "sin"
  Cos _ -> fl "cos"
  AbsF _ -> fl "abs"
  FloorF _ -> fl "floor"
  CeilF _ -> fl "ceil"
  Erf _ -> fl "erf"
  where
    fl name = pure (VF (FUn name [AF (asFloat v)]))
    proj first = case v of
      VPair a b -> if first then a else b
      VVec (a : b : _) -> if first then a else b
      VUnk k -> VUnk (k ++ [AS (if first then "fst" else "snd")])
      _ -> VUnk [AS "proj"]

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

-- | Check every index in a program.  Each top-level value is evaluated, and
-- functions are checked where they are called.
checkProgramBounds :: [Dec Range] -> IO [BoundsIssue]
checkProgramBounds = checkProgramBoundsWith False

-- | Like 'checkProgramBounds'.  When @entries@ is set, a top-level value that
-- is a function is also checked as an entry point, for arbitrary arguments;
-- otherwise functions are checked only where they are called.
checkProgramBoundsWith :: Bool -> [Dec Range] -> IO [BoundsIssue]
checkProgramBoundsWith entries decs = do
  let st0 = St 0 IM.empty IM.empty M.empty M.empty []
      env0 = Env M.empty [] True
      action = do
        env <- foldlM' bindDec M.empty decs
        when entries $
          forM_ [v | Dec _ name [] _ _ _ <- decs, Just v <- [M.lookup name env]] $ \v ->
            withVars env (applyInputs (0 :: Int) v)
      (_, st) = runState (runReaderT action env0) st0
      applyInputs i v = case v of
        VFun f
          | i < 16 -> f (VUnk [AS "input", AN i]) >>= applyInputs (i + 1)
        _ -> pure ()
      obls = nub (reverse (stObls st))
  results <- forM obls (solveObl st)
  pure (concat results)

solveObl :: St -> Obl -> IO [BoundsIssue]
solveObl st (Obl r what path goal) = do
  let seeds = IS.unions (formVars goal : map formVars path)
      vars = closure seeds IS.empty
      facts = concat [IM.findWithDefault [] v (stDefs st) | v <- IS.toList vars]
      info v = IM.findWithDefault (VarInfo "?" False False False) v (stVars st)
      allExact = all (viExact . info) (IS.toList vars)
  debug <- lookupEnv "HYD_BOUNDS_DEBUG"
  let attempt abstract ms = do
        out <- try' (runQuery abstract ms)
        case out of
          Left e -> do
            when (isJust debug) $ hPutStrLn stderr ("bounds check: " ++ show e)
            pure (timedOut e)
          Right x -> pure x
      -- Each truncating division or remainder becomes a fresh variable
      -- constrained only by 'divLemmas'.  A proof under this abstraction is a
      -- proof, but its counterexamples may be spurious.
      runQuery abstract ms = runSMTWith z3 {verbose = False} $ do
        ints <- M.fromList <$> forM [v | v <- IS.toList vars, not (viBool (info v))] (\v -> (v,) <$> sInteger (varName v))
        bools <- M.fromList <$> forM [v | v <- IS.toList vars, viBool (info v)] (\v -> (v,) <$> sBool (varName v))
        let lemmas = divLemmas (goal : path ++ facts)
            divTerms = nub (concatMap divTermsF (goal : path ++ facts ++ lemmas))
        absVars <-
          if abstract
            then M.fromList <$> forM (zip [0 :: Int ..] divTerms) (\(i, t) -> (t,) <$> sInteger ("d" ++ show i))
            else pure M.empty
        let fr = toSBool absVars ints bools
        mapM_ (constrain . fr) facts
        mapM_ (constrain . fr) lemmas
        mapM_ (constrain . fr) path
        constrain (sNot (fr goal))
        query $ do
          cs <- timeout ms checkSat
          case cs of
            Unsat -> pure Nothing
            Sat
              | abstract -> pure (Just Nothing)
              | otherwise -> do
                  vals <-
                    forM
                      [(v, x) | (v, x) <- M.toList ints, viShown (info v)]
                      (\(v, x) -> (v,) <$> getValue x)
                  pure (Just (Just vals))
            _ -> pure (Just Nothing)
  -- Timeouts are in microseconds.
  res1 <- attempt False 4000000
  res <- case res1 of
    Just Nothing -> attempt True 8000000
    _ -> pure res1
  when (isJust debug && (isUnknown res || (isSat res && not allExact))) $ do
    let nm v = viName (info v) ++ "#" ++ show v
    hPutStrLn stderr ("-- undecided: " ++ what)
    hPutStrLn stderr ("   goal: " ++ showF nm goal)
    mapM_ (\f -> hPutStrLn stderr ("   path: " ++ showF nm f)) path
    mapM_ (\f -> hPutStrLn stderr ("   fact: " ++ showF nm f)) facts
  pure $ case res of
    Nothing -> []
    Just Nothing -> [BoundsIssue IssueWarning r what "the solver could not decide it"]
    Just (Just vals)
      | allExact ->
          [ BoundsIssue
              IssueError
              r
              what
              ( "counterexample: "
                  ++ intercalate ", " [dropWhile (== '$') (viName (info v)) ++ " = " ++ show n | (v, n) <- vals]
              )
          ]
      | otherwise ->
          [BoundsIssue IssueWarning r what "it depends on values the checker does not track"]
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try
    timedOut _ = Just Nothing
    isUnknown x = case x of
      Just Nothing -> True
      _ -> False
    isSat x = case x of
      Just (Just _) -> True
      _ -> False
    closure frontier seen
      | IS.null frontier = seen
      | otherwise =
          let seen' = seen <> frontier
              next =
                IS.unions
                  [ formVars f
                  | v <- IS.toList frontier
                  , f <- IM.findWithDefault [] v (stDefs st)
                  ]
           in closure (next `IS.difference` seen') seen'
    varName v = "v" ++ show v

toSInt :: Map T SInteger -> Map Int SInteger -> Map Int SBool -> T -> SInteger
toSInt absVars ints bools t = case M.lookup t absVars of
  Just x -> x
  Nothing -> case t of
    TV v -> M.findWithDefault 0 v ints
    TC n -> literal n
    TAdd a b -> go a + go b
    TSub a b -> go a - go b
    TMul a b -> go a * go b
    TQuot a b -> go a `sQuot` go b
    TRem a b -> go a `sRem` go b
    TIte c a b -> ite (toSBool absVars ints bools c) (go a) (go b)
  where
    go = toSInt absVars ints bools

toSBool :: Map T SInteger -> Map Int SInteger -> Map Int SBool -> F -> SBool
toSBool absVars ints bools f = case f of
  FC True -> sTrue
  FC False -> sFalse
  FV v -> M.findWithDefault sFalse v bools
  FLt a b -> ti a .< ti b
  FLe a b -> ti a .<= ti b
  FEq a b -> ti a .== ti b
  FNot a -> sNot (go a)
  FAnd a b -> go a .&& go b
  FOr a b -> go a .|| go b
  FIff a b -> go a .<=> go b
  where
    go = toSBool absVars ints bools
    ti = toSInt absVars ints bools

-- | Facts about every truncating division and remainder in a set of
-- formulas.  They follow from the definitions and help the solver, which
-- otherwise has to reason about nonlinear division directly.
divLemmas :: [F] -> [F]
divLemmas fs = nub (concatMap lemma (concatMap divTermsF fs))
  where
    lemma t = case t of
      TQuot a b ->
        [ fOr (fNot (fAnd (fLe (TC 0) a) (fLt (TC 0) b)))
            (fAll [fLe (TC 0) t, fLe (tMul t b) a, fLt a (tMul (tAdd t (TC 1)) b)])
        ]
      TRem a b ->
        [ fOr (fNot (fAnd (fLe (TC 0) a) (fLt (TC 0) b)))
            (fAll [fLe (TC 0) t, fLt t b, fLe t a])
        ]
      _ -> []

divTermsF :: F -> [T]
divTermsF f = case f of
  FC _ -> []
  FV _ -> []
  FLt a b -> divTermsT a ++ divTermsT b
  FLe a b -> divTermsT a ++ divTermsT b
  FEq a b -> divTermsT a ++ divTermsT b
  FNot a -> divTermsF a
  FAnd a b -> divTermsF a ++ divTermsF b
  FOr a b -> divTermsF a ++ divTermsF b
  FIff a b -> divTermsF a ++ divTermsF b

divTermsT :: T -> [T]
divTermsT t = case t of
  TV _ -> []
  TC _ -> []
  TAdd a b -> divTermsT a ++ divTermsT b
  TSub a b -> divTermsT a ++ divTermsT b
  TMul a b -> divTermsT a ++ divTermsT b
  TQuot a b -> t : divTermsT a ++ divTermsT b
  TRem a b -> t : divTermsT a ++ divTermsT b
  TIte c a b -> divTermsF c ++ divTermsT a ++ divTermsT b

showT :: (Int -> String) -> T -> String
showT nm t = case t of
  TV v -> nm v
  TC n -> show n
  TAdd a b -> bin "+" a b
  TSub a b -> bin "-" a b
  TMul a b -> bin "*" a b
  TQuot a b -> bin "/" a b
  TRem a b -> bin "%" a b
  TIte c a b -> "(if " ++ showF nm c ++ " then " ++ showT nm a ++ " else " ++ showT nm b ++ ")"
  where
    bin o a b = "(" ++ showT nm a ++ " " ++ o ++ " " ++ showT nm b ++ ")"

showF :: (Int -> String) -> F -> String
showF nm f = case f of
  FC b -> show b
  FV v -> nm v
  FLt a b -> showT nm a ++ " < " ++ showT nm b
  FLe a b -> showT nm a ++ " <= " ++ showT nm b
  FEq a b -> showT nm a ++ " = " ++ showT nm b
  FNot a -> "!(" ++ showF nm a ++ ")"
  FAnd a b -> "(" ++ showF nm a ++ " & " ++ showF nm b ++ ")"
  FOr a b -> "(" ++ showF nm a ++ " | " ++ showF nm b ++ ")"
  FIff a b -> "(" ++ showF nm a ++ " <-> " ++ showF nm b ++ ")"
