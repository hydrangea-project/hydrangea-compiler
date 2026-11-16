{-# LANGUAGE LambdaCase #-}

-- | When may a let-bound (or beta-bound) expression be substituted at its use
-- sites?
--
-- Scatters, reductions and permutes leave their contributor order
-- unspecified, so two evaluations of the same term may differ.  @let x = e in
-- b@ evaluates @e@ once; substituting @e@ for @x@ is only sound when the
-- copies cannot be evaluated more than once, or when @e@ cannot accumulate.
-- 'dupSafe' is the side condition every inliner and beta-reducer in fusion
-- checks: the expression is a value, the variable is used at most once
-- outside any repeated context, or the expression does not accumulate.
module Language.Hydrangea.DupSafe
  ( AccumEnv
  , emptyAccumEnv
  , accumEnvDecs
  , accumEnvExp
  , mayAccumulate
  , isValueExp
  , usedOnceOutsideLoops
  , dupSafe
  , dupSafePat
  ) where

import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.Syntax

-- | Names of functions (and function-valued bindings) whose invocation may
-- evaluate an accumulation, together with the known arity of every
-- function-defining declaration.
data AccumEnv = AccumEnv
  { aeTainted :: Set Var
  , aeArity   :: M.Map Var Int
  }

emptyAccumEnv :: AccumEnv
emptyAccumEnv = AccumEnv S.empty M.empty

-- | Build the environment for a whole program: every declaration, top-level
-- or nested, contributes.  Names are unique after uniquification, so one
-- global set is enough; a shadowed name can only make the result more
-- conservative.
accumEnvDecs :: [Dec a] -> AccumEnv
accumEnvDecs decs = accumEnvFrom (concatMap decsInDec decs) (concatMap appsInDec decs)

-- | Like 'accumEnvDecs', for one expression, on top of an outer environment
-- (typically the program's top-level one).
accumEnvExp :: AccumEnv -> Exp a -> AccumEnv
accumEnvExp outer e =
  let inner = accumEnvFrom (decsInExp e) (appsInExp e)
   in AccumEnv (aeTainted outer `S.union` aeTainted inner)
               (aeArity inner `M.union` aeArity outer)

accumEnvFrom :: [Dec a] -> [(Var, [Exp a])] -> AccumEnv
accumEnvFrom decs apps = go S.empty
  where
    arity = M.fromList [(n, length ps) | Dec _ n ps _ _ _ <- decs, not (null ps)]
    params = M.fromList [(n, ps) | Dec _ n ps _ _ _ <- decs, not (null ps)]

    go tainted =
      let env = AccumEnv tainted arity
          fromDecs = S.fromList
            [ n
            | Dec _ n ps _ _ body <- decs
            , if null ps then fnValueTainted env body else mayAccumulate env body
            ]
          -- A parameter bound to a tainted function at some call site is
          -- itself tainted.
          fromParams = S.fromList
            [ v
            | (f, args) <- apps
            , Just ps <- [M.lookup f params]
            , (p, arg) <- zip ps args
            , fnValueTainted env arg
            , v <- patVars p
            ]
          tainted' = S.unions [tainted, fromDecs, fromParams]
       in if tainted' == tainted then env else go tainted'

-- | Does evaluating this expression possibly yield a function whose
-- invocation accumulates?  Over-approximates only where it has to: a full
-- application of a known function yields its result, not a closure.
fnValueTainted :: AccumEnv -> Exp a -> Bool
fnValueTainted env = \case
  EVar _ v -> v `S.member` aeTainted env
  e@(EApp {}) -> case peel e [] of
    (EVar _ v, args) ->
      v `S.member` aeTainted env
        && maybe True (length args <) (M.lookup v (aeArity env))
    (hd, _) -> fnValueTainted env hd
  ELetIn _ _ body -> fnValueTainted env body
  EIfThen _ _ t -> fnValueTainted env t
  EIfThenElse _ _ t f -> fnValueTainted env t || fnValueTainted env f
  EVec _ es -> any (fnValueTainted env) es
  EPair _ x y -> fnValueTainted env x || fnValueTainted env y
  ERecord _ fs -> any (fnValueTainted env . snd) fs
  EProj _ _ x -> fnValueTainted env x
  ERecordProj _ x _ -> fnValueTainted env x
  _ -> False
  where
    peel (EApp _ f x) acc = peel f (x : acc)
    peel f acc = (f, acc)

-- | Can evaluating this expression perform a nondeterministic accumulation
-- (or a side effect)?  Conservative: any mention of a tainted name counts.
mayAccumulate :: AccumEnv -> Exp a -> Bool
mayAccumulate env e =
  any accumulatingNode (universe e)
    || any (`S.member` aeTainted env) [v | EVar _ v <- universe e]

-- | Constructors whose result is not determined by their inputs: the
-- accumulation order is unspecified (and floating-point combines observe
-- it), the write order races, or evaluation has an effect.  Scans and the
-- sparse helpers are included conservatively.
accumulatingNode :: Exp a -> Bool
accumulatingNode = \case
  EReduce {}           -> True
  EReduceGenerate {}   -> True
  EScan {}             -> True
  EScanInclusive {}    -> True
  EScanR {}            -> True
  EScanRInclusive {}   -> True
  ESegmentedReduce {}  -> True
  ECOOSumDuplicates {} -> True
  EPermute {}          -> True
  EScatter {}          -> True
  EScatterGuarded {}   -> True
  EScatterGenerate {}  -> True
  EScatterChain {}     -> True
  EScatterGen {}       -> True
  EWriteArray {}       -> True
  EWriteArrayFloat {}  -> True
  _                    -> False

-- | Syntactic values: evaluating them does nothing, so copying them is free
-- of semantic risk.  A local function @let f ps = body in f@ is a closure.
isValueExp :: Exp a -> Bool
isValueExp = \case
  EVar {}    -> True
  EInt {}    -> True
  EFloat {}  -> True
  EBool {}   -> True
  EUnit {}   -> True
  EString {} -> True
  EOp {}     -> True
  ENeg _ x   -> isValueExp x
  EVec _ es  -> all isValueExp es
  EPair _ x y -> isValueExp x && isValueExp y
  ERecord _ fs -> all (isValueExp . snd) fs
  ELetIn _ (Dec _ f ps _ _ _) (EVar _ f') -> not (null ps) && f == f'
  _ -> False

-- | Is @v@ used at most once in @e@, and never inside a context that runs
-- repeatedly (a local function body, or the function argument of an array
-- combinator, which runs once per element)?
usedOnceOutsideLoops :: Var -> Exp a -> Bool
usedOnceOutsideLoops v e = occ False e <= 1
  where
    -- An occurrence in a repeated context counts as "many".
    hit rep = if rep then 2 else 1

    occ :: Bool -> Exp a -> Int
    occ rep ex = case ex of
      EVar _ x | x == v -> hit rep
      ELetIn _ (Dec _ _ ps _ _ body) rest ->
        occ (rep || not (null ps)) body + occ rep rest
      EGenerate _ sz f -> occ rep sz + occ True f
      EMap _ f arr -> occ True f + occ rep arr
      EZipWith _ f x y -> occ True f + occ rep x + occ rep y
      EReduce _ f z arr -> occ True f + occ rep z + occ rep arr
      EReduceGenerate _ f z sh g -> occ True f + occ rep z + occ rep sh + occ True g
      EIterate _ n i f -> occ rep n + occ rep i + occ True f
      EFoldl _ f z arr -> occ True f + occ rep z + occ rep arr
      EFoldlWhile _ p f z arr -> occ True p + occ True f + occ rep z + occ rep arr
      EScan _ f z arr -> occ True f + occ rep z + occ rep arr
      EScanInclusive _ f z arr -> occ True f + occ rep z + occ rep arr
      EScanR _ f z arr -> occ True f + occ rep z + occ rep arr
      EScanRInclusive _ f z arr -> occ True f + occ rep z + occ rep arr
      ESegmentedReduce _ f z o vs -> occ True f + occ rep z + occ rep o + occ rep vs
      EPermute _ c d p arr -> occ True c + occ rep d + occ rep p + occ rep arr
      EScatter _ c d i x -> occ True c + occ rep d + occ rep i + occ rep x
      EScatterGuarded _ c d i x g -> occ True c + occ rep d + occ rep i + occ rep x + occ rep g
      EScatterGenerate _ c d i g -> occ True c + occ rep d + occ rep i + occ True g
      EScatterChain _ c d phases ->
        occ True c + occ rep d + sum (map (occ rep) (concatMap phaseExps phases))
      EScatterGen _ c d phases ->
        occ True c + occ rep d
          + sum [ occ rep (sgpShape ph) + occ True (sgpIndexFn ph) + occ True (sgpValueFn ph)
                    + maybe 0 (occ True) (sgpGuardFn ph)
                | ph <- phases ]
      EStencil _ bc f arr -> sum (map (occ rep) (bcExps bc)) + occ True f + occ rep arr
      _ -> sum (map (occ rep) (children ex))

-- | May @e@ be substituted for @v@ in @body@?
dupSafe :: AccumEnv -> Var -> Exp a -> Exp a -> Bool
dupSafe env v e body =
  isValueExp e || usedOnceOutsideLoops v body || not (mayAccumulate env e)

-- | 'dupSafe' for a pattern binding.  A compound pattern projects the
-- argument once per component, which copies it even when each component is
-- used once, so only a value or a non-accumulating argument qualifies.
dupSafePat :: AccumEnv -> Pat a -> Exp a -> Exp a -> Bool
dupSafePat env p arg body = case p of
  PVar _ v -> dupSafe env v arg body
  _        -> isValueExp arg || not (mayAccumulate env arg)

------------------------------------------------------------------------
-- Traversal helpers
------------------------------------------------------------------------

patVars :: Pat a -> [Var]
patVars = \case
  PVar _ v     -> [v]
  PVec _ ps    -> concatMap patVars ps
  PBound _ v _ -> [v]
  PPair _ p q  -> patVars p ++ patVars q

decsInDec :: Dec a -> [Dec a]
decsInDec d@(Dec _ _ _ _ _ body) = d : decsInExp body

decsInExp :: Exp a -> [Dec a]
decsInExp e = [d | ELetIn _ d _ <- universe e]

appsInDec :: Dec a -> [(Var, [Exp a])]
appsInDec (Dec _ _ _ _ _ body) = appsInExp body

-- | Every application spine @f a1 ... an@ headed by a variable.
appsInExp :: Exp a -> [(Var, [Exp a])]
appsInExp e = [(f, args) | x@(EApp {}) <- universe e, (EVar _ f, args) <- [peel x []]]
  where
    peel (EApp _ f x) acc = peel f (x : acc)
    peel f acc = (f, acc)

-- | The expression and all of its sub-expressions.
universe :: Exp a -> [Exp a]
universe e = e : concatMap universe (children e)

phaseExps :: ScatterPhase a -> [Exp a]
phaseExps ph = [spIndex ph, spValues ph] ++ maybe [] pure (spGuard ph)

bcExps :: BoundaryCondition a -> [Exp a]
bcExps = \case
  BConst x -> [x]
  _        -> []

-- | Immediate sub-expressions.  Deliberately total: a missed constructor
-- would hide an accumulation from 'mayAccumulate'.
children :: Exp a -> [Exp a]
children = \case
  EInt {}    -> []
  EFloat {}  -> []
  EVar {}    -> []
  EString {} -> []
  EUnit {}   -> []
  EBool {}   -> []
  EOp {}     -> []
  EVec _ es -> es
  EApp _ f x -> [f, x]
  EIfThen _ c t -> [c, t]
  EIfThenElse _ c t f -> [c, t, f]
  ENeg _ x -> [x]
  EBinOp _ l _ r -> [l, r]
  EUnOp _ _ x -> [x]
  ELetIn _ (Dec _ _ ps _ _ b) body -> concatMap patExps ps ++ [b, body]
  EBoundLetIn _ _ bnd rhs body -> [bnd, rhs, body]
  EProj _ _ x -> [x]
  EPair _ x y -> [x, y]
  ERecord _ fs -> map snd fs
  ERecordProj _ x _ -> [x]
  EGenerate _ sz f -> [sz, f]
  EMap _ f arr -> [f, arr]
  EZipWith _ f x y -> [f, x, y]
  EAppend _ x y -> [x, y]
  EReduce _ f z arr -> [f, z, arr]
  EReduceGenerate _ f z sh g -> [f, z, sh, g]
  EIterate _ n i f -> [n, i, f]
  EFoldl _ f z arr -> [f, z, arr]
  EFoldlWhile _ p f z arr -> [p, f, z, arr]
  EScan _ f z arr -> [f, z, arr]
  EScanInclusive _ f z arr -> [f, z, arr]
  EScanR _ f z arr -> [f, z, arr]
  EScanRInclusive _ f z arr -> [f, z, arr]
  ESegmentedReduce _ f z o vs -> [f, z, o, vs]
  ESortIndices _ arr -> [arr]
  EIota _ n -> [n]
  EMakeIndex _ n arr -> [n, arr]
  ECOOSumDuplicates _ a b c d f g -> [a, b, c, d, f, g]
  ECSRFromSortedCOO _ a b c d f g -> [a, b, c, d, f, g]
  EPermute _ c d p arr -> [c, d, p, arr]
  EScatter _ c d i x -> [c, d, i, x]
  EScatterGuarded _ c d i x g -> [c, d, i, x, g]
  EScatterGenerate _ c d i g -> [c, d, i, g]
  EScatterChain _ c d phases -> c : d : concatMap phaseExps phases
  EScatterGen _ c d phases ->
    c : d : concat [ [sgpShape ph, sgpIndexFn ph, sgpValueFn ph] ++ maybe [] pure (sgpGuardFn ph)
                   | ph <- phases ]
  EGather _ i arr -> [i, arr]
  EIndex _ i arr -> [i, arr]
  ECheckIndex _ i d arr -> [i, d, arr]
  EFill _ sz v -> [sz, v]
  EShapeOf _ arr -> [arr]
  EReplicate _ dims arr -> concatMap shapeDimExps dims ++ [arr]
  ESlice _ dims arr -> concatMap sliceDimExps dims ++ [arr]
  EReshape _ sz arr -> [sz, arr]
  EStencil _ bc f arr -> bcExps bc ++ [f, arr]
  EReify _ arr -> [arr]
  EReadArray _ sz f -> [sz, f]
  EReadArrayFloat _ sz f -> [sz, f]
  EWriteArray _ arr f -> [arr, f]
  EWriteArrayFloat _ arr f -> [arr, f]
  EGetEnvInt _ x -> [x]
  EGetEnvString _ x -> [x]
  where
    shapeDimExps = \case
      ShapeAll _   -> []
      ShapeAny _ x -> [x]
      ShapeDim _ x -> [x]
    sliceDimExps = \case
      SliceAll _       -> []
      SliceRange _ s l -> [s, l]
    patExps = \case
      PBound _ _ x -> [x]
      PVec _ ps    -> concatMap patExps ps
      PPair _ p q  -> patExps p ++ patExps q
      _            -> []
