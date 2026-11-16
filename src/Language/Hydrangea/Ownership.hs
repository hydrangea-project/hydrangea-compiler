{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Ownership
--
-- Reference-counted ownership of array storage for the C backend.
--
-- The runtime counts references to array storage, one per header
-- (@hyd_array_retain@ makes a new header, @hyd_array_free@ drops one).  This
-- pass makes the counts exact by inserting a retain wherever a second live
-- reference is made and a release right after each reference's last use.
-- It is the only thing that frees arrays: no earlier pass emits a free.  It
-- runs on each procedure just before C code generation, after every CFG
-- optimisation, so no later pass can move a statement across an inserted
-- one.
--
-- == What is managed
--
-- A variable is /managed/ when its type holds arrays (an array, or a pair or
-- record with an array somewhere inside), it is not a parameter, and every
-- assignment to it is serial: at the top level of the procedure, in an @if@,
-- or in a serial loop, but not inside a parallel loop or region.  (No array
-- is made inside those: their bodies work on elements.)
--
-- Every managed variable owns one reference per array it holds.  Parameters
-- are borrowed: the caller keeps its reference, and a procedure returns a
-- reference its caller owns.
--
-- == Rules
--
-- * An allocation, a call or a view produces an owned value.
-- * A copy @w = v@, a pair or record component, and a projection move the
--   reference when the source dies at that statement, and retain otherwise.
--   Moving a component out of a dying pair releases the pair's other
--   components.
-- * @hyd_array_own(v)@ consumes @v@ the same way (it returns @v@ itself when
--   its storage is unshared, and a copy otherwise).
-- * A managed variable is released right after its last use, on each path:
--   an @if@ releases, at the start of each branch, what that branch doesn't
--   use but the other one does.
-- * A value returned from the procedure moves out; a borrowed one is
--   retained first.
-- * An array stored into an element of another array is owned by that
--   element: the store moves the reference when the stored variable dies
--   there, and retains otherwise.  An array loaded out of an element is
--   borrowed from it, so the load retains.  Loops that store arrays are
--   never parallel ('storesArrays'), so the retains are serial.
--
-- == Loops
--
-- A serial loop owns the managed variables live at its header: those live
-- after the loop, and those its body reads before writing (a fixpoint, since
-- the body's end flows back to the header).  An iterate's state is one of
-- these.  Each trip starts by releasing the header variables its body
-- overwrites before reading, and after the loop (reached from the header or
-- by a @break@, where everything live at the header is live too) the header
-- variables dead afterwards are released.  Inside the body the rules above
-- apply unchanged, so a per-trip temporary is released at its last use in
-- the trip, and a new state replacing the old one releases what the old one
-- held that the new one doesn't.
--
-- Parallel loops and regions borrow what they read.
--
-- Zero-argument memoised array calls inside a loop return a new view on each
-- call; they are hoisted out of the loop first, as codegen would do anyway,
-- so that one view serves every trip.
module Language.Hydrangea.Ownership
  ( OwnershipResult (..),
    insertOwnership,
    hasArrayLeaves,
  )
where

import Control.Monad (forM, foldM)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis (hasArrayLeaves, storesArrays, usedVarsAtom, usedVarsIndexExpr, usedVarsRHS, usedVarsStmt)
import Language.Hydrangea.CFGCore (Atom (..), CType (..), RHS (..), ctypeToElemType)
import Language.Hydrangea.CFGOpt (substRHS)

newtype OwnershipResult = OwnershipResult
  { -- | The procedure with retains and releases inserted, and the types of
    -- the temporaries it introduced added to its type environment.
    orProc :: Proc
  }

-- | Can the pass retain and release values of this type?  Every pair on the
-- way to an array must be expressible as a pair element.
manageable :: CType -> Bool
manageable ct = case ct of
  CTArray _ -> True
  CTPair _ _ -> isJust (ctypeToElemType ct)
  CTRecord fs -> all (\(_, t) -> not (hasArrayLeaves t) || manageable t) fs
  _ -> False

data Sel = SelFst | SelSnd | SelField ByteString
  deriving (Eq, Ord)

-- | The components of an aggregate type: selector, type, and projection.
components :: CType -> [(Sel, CType, Atom -> RHS)]
components ct = case ct of
  CTPair a b
    | Just ea <- ctypeToElemType a,
      Just eb <- ctypeToElemType b ->
        [(SelFst, a, RPairFst ea), (SelSnd, b, RPairSnd eb)]
  CTRecord fs -> [(SelField f, t, RRecordProj f) | (f, t) <- fs]
  _ -> []

rebuild :: CType -> [Atom] -> RHS
rebuild ct atoms = case (ct, atoms) of
  (CTPair a b, [x, y])
    | Just ea <- ctypeToElemType a,
      Just eb <- ctypeToElemType b ->
        RPairMake ea eb x y
  (CTRecord fs, _) -> RRecord (zip (map fst fs) atoms)
  _ -> error "Ownership.rebuild: not an aggregate"

data St = St
  { stNext :: !Int,
    stNewTypes :: !(Map CVar CType),
    stManaged :: !(Set CVar)
  }

type M = State St

data Env = Env
  { envTypes :: Map CVar CType,
    -- | Unmanaged variables that may alias managed ones.
    envRoots :: Map CVar (Set CVar),
    -- | Managed variables live where a @break@ jumps to: the header of the
    -- innermost enclosing loop.
    envBreakLive :: Set CVar,
    -- | Aggregates taken apart by projections, with the components those
    -- projections move out (see 'destructured').
    envDestructured :: Map CVar (Set Sel)
  }

typeOf :: Env -> St -> CVar -> Maybe CType
typeOf env st v = M.lookup v (stNewTypes st) `orElse` M.lookup v (envTypes env)
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

fresh :: CType -> M CVar
fresh ct = do
  n <- gets stNext
  let v = BS.pack ("__rc" ++ show n)
  modify' (\st -> st {stNext = n + 1, stNewTypes = M.insert v ct (stNewTypes st)})
  pure v

-- | A fresh variable the pass itself owns (a renamed old value).
freshManaged :: CType -> M CVar
freshManaged ct = do
  v <- fresh ct
  modify' (\st -> st {stManaged = S.insert v (stManaged st)})
  pure v

-- | Statements that make @x@ a new owned reference to the value of @a@.
retainInto :: CVar -> CType -> Atom -> M [Stmt]
retainInto x ct a = case ct of
  CTArray _ -> pure [SAssign x (RCall "hyd_array_retain" [a])]
  _ -> do
    parts <- forM (components ct) $ \(_, t, proj) -> do
      p <- fresh t
      if hasArrayLeaves t
        then do
          r <- fresh t
          ss <- retainInto r t (AVar p)
          pure (SAssign p (proj a) : ss, AVar r)
        else pure ([SAssign p (proj a)], AVar p)
    pure (concatMap fst parts ++ [SAssign x (rebuild ct (map snd parts))])

-- | Statements that release every array reference held by @a@.
releaseAtom :: CType -> Atom -> M [Stmt]
releaseAtom ct a = case ct of
  CTArray _ -> pure [SAssign "__hyd_discard" (RArrayFree a)]
  _ -> fmap concat $
    forM [c | c@(_, t, _) <- components ct, hasArrayLeaves t] $ \(_, t, proj) -> do
      p <- fresh t
      rest <- releaseAtom t (AVar p)
      pure (SAssign p (proj a) : rest)

insertOwnership ::
  -- | Is this statement a call to a memoised zero-argument array proc?
  (Stmt -> Bool) ->
  -- | The procedure's recovered type environment.
  Map CVar CType ->
  Proc ->
  OwnershipResult
insertOwnership isMemoCall types proc
  | hasFree (procBody proc) =
      error ("Ownership: " ++ BS.unpack (procName proc) ++ " already frees an array")
  | otherwise =
      let params = S.fromList (procParams proc)
          body0 = hoistMemoCalls isMemoCall (procBody proc)
          candidates =
            S.filter
              ( \v ->
                  v `S.notMember` params
                    && maybe False (\t -> hasArrayLeaves t && manageable t) (M.lookup v types)
              )
              (serialAssigned body0 `S.difference` parallelAssigned body0)
          managed = candidates `S.difference` borrowed types params candidates body0
          env =
            Env
              { envTypes = types,
                envRoots = aliasRoots types managed body0,
                envBreakLive = S.empty,
                envDestructured = destructured types managed body0
              }
          st0 = St {stNext = 0, stNewTypes = M.empty, stManaged = managed}
          ((body', _), st') = runState (goBlock env S.empty body0) st0
       in OwnershipResult
            { orProc =
                proc
                  { procBody = body',
                    procTypeEnv = M.union (stNewTypes st') (procTypeEnv proc)
                  }
            }

-- | Candidates that can borrow instead of owning: copies and projections
-- that never outlive what they borrow from.
--
-- A variable assigned once, by a copy or projection of another array
-- variable, is left unmanaged (an alias, see 'aliasRoots') when, at every
-- point where it is live, each managed root it aliases is live too and is
-- not being reassigned.  Its uses then never extend a root's life, so
-- treating it as an alias costs nothing, and it saves the retain and
-- release a managed copy would need: an iterate body that takes its state
-- pair apart while the state stays live, say.  Where the root dies first (a
-- projection out of a pair that dies at the projection, for one) the copy
-- stays managed and takes the reference over.
borrowed :: Map CVar CType -> Set CVar -> Set CVar -> [Stmt] -> Set CVar
borrowed types params candidates body = go initial
  where
    (points, defLive) = liveness body
    defCounts = M.fromListWith (+) [(v, 1 :: Int) | (v, _) <- assignments body]
    initial =
      S.fromList
        [ v
        | (v, rhs) <- assignments body,
          v `S.member` candidates,
          M.lookup v defCounts == Just 1,
          Just src <- [copySource rhs],
          src `S.member` candidates || src `S.member` params
        ]
    go bs =
      let roots = aliasRoots types (candidates `S.difference` bs) body
          ok v = all (safeFrom v) (S.toList (M.findWithDefault S.empty v roots))
          safeFrom v r =
            all (\live -> v `S.notMember` live || r `S.member` live) points
              && all (v `S.notMember`) (M.findWithDefault [] r defLive)
          bs' = S.filter ok bs
       in if bs' == bs then bs else go bs'

    copySource rhs = case rhs of
      RAtom (AVar v) -> Just v
      RPairFst _ (AVar v) -> Just v
      RPairSnd _ (AVar v) -> Just v
      RProj _ (AVar v) -> Just v
      RRecordProj _ (AVar v) -> Just v
      _ -> Nothing

-- | Aggregates that are taken apart component by component.
--
-- A managed pair or record assigned once, whose only uses are projections
-- of distinct components by a run of consecutive assignments in the block
-- that defines it, can hand each array component to its projection instead
-- of retaining it: nothing else reads the aggregate, and no branch or loop
-- sits between the projections to release it half taken apart.  (The last
-- projection releases the array components none of them takes.)  Maps each
-- such aggregate to the components its projections move out.
destructured :: Map CVar CType -> Set CVar -> [Stmt] -> Map CVar (Set Sel)
destructured types managed body = M.fromList (concatMap inBlock (blocks body))
  where
    tyOf v = M.lookup v types
    blocks stmts = stmts : concatMap sub stmts
    sub st = case st of
      SLoop _ b -> blocks b
      SParallelRegion b -> blocks b
      SIf _ t e -> blocks t ++ blocks e
      _ -> []
    defCounts = M.fromListWith (+) [(v, 1 :: Int) | (v, _) <- assignments body]
    mentions = M.fromListWith (+) [(v, 1 :: Int) | us <- leafUses body, v <- S.toList us]

    inBlock stmts =
      [ (p, S.fromList [sel | (_, _, sel) <- arrayProjs])
      | (d, SAssign p _) <- indexed,
        p `S.member` managed,
        M.lookup p defCounts == Just 1,
        let comps = maybe [] componentTypes (tyOf p),
        not (null comps),
        let projs =
              [ (i, x, sel)
              | (i, SAssign x rhs) <- indexed,
                i > d,
                Just (p', sel) <- [projection tyOf rhs],
                p' == p
              ],
        let arrayProjs = [pr | pr@(_, _, sel) <- projs, maybe False hasArrayLeaves (lookup sel comps)],
        length arrayProjs >= 2,
        M.lookup p mentions == Just (length projs),
        distinct [sel | (_, _, sel) <- projs],
        all (\(_, x, _) -> x `S.member` managed) arrayProjs,
        let (firstIx, lastIx) = (minimum [i | (i, _, _) <- projs], maximum [i | (i, _, _) <- projs]),
        any (\(i, _, _) -> i == lastIx) arrayProjs,
        all isAssign [st | (i, st) <- indexed, i >= firstIx, i <= lastIx]
      ]
      where
        indexed = zip [0 :: Int ..] stmts

    distinct xs = S.size (S.fromList xs) == length xs
    isAssign st = case st of
      SAssign {} -> True
      _ -> False

-- | The variables each leaf statement (and each @if@ condition and loop
-- bound) reads, one set per occurrence.
leafUses :: [Stmt] -> [Set CVar]
leafUses = concatMap go
  where
    go stmt = case stmt of
      SLoop spec b -> foldMap usedVarsIndexExpr (lsBounds spec ++ lsOrigins spec ++ maybe [] (pure . rsInit) (lsRed spec)) : leafUses b
      SParallelRegion b -> leafUses b
      SIf c t e -> usedVarsAtom c : leafUses t ++ leafUses e
      _ -> [usedVarsStmt stmt]

-- | Every assignment, anywhere.
assignments :: [Stmt] -> [(CVar, RHS)]
assignments = concatMap go
  where
    go stmt = case stmt of
      SAssign v rhs -> [(v, rhs)]
      SLoop _ b -> assignments b
      SParallelRegion b -> assignments b
      SIf _ t e -> assignments t ++ assignments e
      _ -> []

-- | Plain variable liveness: the live set after every statement and at the
-- start of every block, and, for each variable, the live sets after its
-- assignments.
liveness :: [Stmt] -> ([Set CVar], Map CVar [Set CVar])
liveness body = let (_, pts, defs) = block S.empty S.empty body in (pts, defs)
  where
    block brk out stmts =
      let step (live, ps, ds) st =
            let (live', ps', ds') = stmt brk live st
             in (live', live : ps' ++ ps, M.unionWith (++) ds' ds)
          (liveIn, pts, defs) = foldl step (out, [], M.empty) (reverse stmts)
       in (liveIn, liveIn : pts, defs)

    stmt brk out st = case st of
      SAssign x rhs -> (S.delete x out <> usedVarsRHS rhs, [], M.singleton x [out])
      SArrayWrite {} -> (out <> usedVarsStmt st, [], M.empty)
      SIf c t e ->
        let (lt, pt, dt) = block brk out t
            (le, pe, de) = block brk out e
         in (usedVarsAtom c <> lt <> le, pt ++ pe, M.unionWith (++) dt de)
      SLoop spec b ->
        let specUses = foldMap usedVarsIndexExpr (lsBounds spec ++ lsOrigins spec ++ maybe [] (pure . rsInit) (lsRed spec))
            entry = out <> specUses
            header h =
              let (lb, _, _) = block h h b
                  h' = entry <> lb
               in if h' == h then h else header h'
            hdr = header entry
            (_, pb, db) = block hdr hdr b
         in (hdr, hdr : pb, db)
      SParallelRegion b -> block brk out b
      SReturn a -> (usedVarsAtom a, [], M.empty)
      SBreak -> (brk, [], M.empty)

-- | Does any statement free an array explicitly?
hasFree :: [Stmt] -> Bool
hasFree = any go
  where
    go stmt = case stmt of
      SAssign _ (RArrayFree _) -> True
      SLoop _ body -> hasFree body
      SParallelRegion body -> hasFree body
      SIf _ thn els -> hasFree thn || hasFree els
      _ -> False

isParallelLoop :: LoopSpec -> Bool
isParallelLoop spec = case lsExec spec of
  Parallel {} -> True
  Workshare {} -> True
  _ -> False

-- | Every variable assigned anywhere.
allAssigned :: [Stmt] -> Set CVar
allAssigned = foldMap go
  where
    go stmt = case stmt of
      SAssign v _ -> S.singleton v
      SLoop _ body -> allAssigned body
      SParallelRegion body -> allAssigned body
      SIf _ thn els -> allAssigned thn <> allAssigned els
      _ -> S.empty

-- | Variables assigned outside every parallel loop and region.
serialAssigned :: [Stmt] -> Set CVar
serialAssigned = foldMap go
  where
    go stmt = case stmt of
      SAssign v _ -> S.singleton v
      SIf _ thn els -> serialAssigned thn <> serialAssigned els
      SLoop spec body | not (isParallelLoop spec) -> serialAssigned body
      _ -> S.empty

-- | Variables assigned inside some parallel loop or region.
parallelAssigned :: [Stmt] -> Set CVar
parallelAssigned = foldMap go
  where
    go stmt = case stmt of
      SLoop spec body
        | isParallelLoop spec -> allAssigned body
        | otherwise -> parallelAssigned body
      SParallelRegion body -> allAssigned body
      SIf _ thn els -> parallelAssigned thn <> parallelAssigned els
      _ -> S.empty

-- | For each unmanaged variable, the managed variables whose arrays it may
-- hold without owning them: those it copies, projects or pairs up, directly
-- or through other unmanaged variables.  A variable whose type is known to
-- hold no array (a scalar projected out of a pair) aliases nothing.
aliasRoots :: Map CVar CType -> Set CVar -> [Stmt] -> Map CVar (Set CVar)
aliasRoots types managed body = fixpoint M.empty
  where
    edges =
      [ (w, srcs)
      | (w, rhs) <- assigns body,
        w `S.notMember` managed,
        maybe True hasArrayLeaves (M.lookup w types),
        let srcs = aliasSources rhs,
        not (null srcs)
      ]
    step roots =
      M.unionsWith
        S.union
        ( roots
            : [ M.singleton w (S.unions [rootsOf roots v | v <- srcs])
              | (w, srcs) <- edges
              ]
        )
    rootsOf roots v
      | v `S.member` managed = S.singleton v
      | otherwise = M.findWithDefault S.empty v roots
    fixpoint roots =
      let roots' = step roots in if roots' == roots then roots else fixpoint roots'

    assigns = concatMap go
      where
        go stmt = case stmt of
          SAssign v rhs -> [(v, rhs)]
          SLoop _ b -> assigns b
          SParallelRegion b -> assigns b
          SIf _ t e -> assigns t ++ assigns e
          _ -> []

    aliasSources rhs = case rhs of
      RAtom (AVar v) -> [v]
      RPairFst _ (AVar v) -> [v]
      RPairSnd _ (AVar v) -> [v]
      RProj _ (AVar v) -> [v]
      RRecordProj _ (AVar v) -> [v]
      RPairMake _ _ a b -> [v | AVar v <- [a, b]]
      RRecord fs -> [v | (_, AVar v) <- fs]
      _ -> []

-- | Hoist memoised zero-argument array calls out of the loops and parallel
-- regions at procedure scope (not out of an @if@ inside them), the way
-- codegen hoists them out of each loop it emits.
hoistMemoCalls :: (Stmt -> Bool) -> [Stmt] -> [Stmt]
hoistMemoCalls isMemoCall = concatMap top
  where
    top stmt = case stmt of
      SLoop spec body -> let (h, k) = pull body in h ++ [SLoop spec k]
      SParallelRegion body -> let (h, k) = pull body in h ++ [SParallelRegion k]
      SIf c thn els -> [SIf c (hoistMemoCalls isMemoCall thn) (hoistMemoCalls isMemoCall els)]
      _ -> [stmt]

    pull = foldr step ([], [])
      where
        step stmt (hs, ks)
          | isMemoCall stmt = (stmt : hs, ks)
          | otherwise = case stmt of
              SLoop spec body -> let (h, k) = pull body in (h ++ hs, SLoop spec k : ks)
              SParallelRegion body -> let (h, k) = pull body in (h ++ hs, SParallelRegion k : ks)
              _ -> (hs, stmt : ks)

-- | Managed variables a set of used variables keeps alive.
mapUses :: Env -> Set CVar -> Set CVar -> Set CVar
mapUses env managed = foldMap one
  where
    one v
      | v `S.member` managed = S.singleton v
      | otherwise = M.findWithDefault S.empty v (envRoots env)

releaseVars :: Env -> Set CVar -> M [Stmt]
releaseVars env vs = do
  st <- gets id
  fmap concat $
    forM (S.toList vs) $ \v -> case typeOf env st v of
      Just ct -> releaseAtom ct (AVar v)
      Nothing -> pure []

-- | Process a block backwards from the variables live after it; return the
-- rewritten block and the managed variables live before it.
goBlock :: Env -> Set CVar -> [Stmt] -> M ([Stmt], Set CVar)
goBlock env liveOut stmts = foldM step ([], liveOut) (reverse stmts)
  where
    step (acc, live) stmt = do
      (stmts', liveIn) <- goStmt env live stmt
      pure (stmts' ++ acc, liveIn)

endsInReturn :: [Stmt] -> Bool
endsInReturn stmts = case reverse stmts of
  SReturn _ : _ -> True
  _ -> False

goStmt :: Env -> Set CVar -> Stmt -> M ([Stmt], Set CVar)
goStmt env liveOut stmt = do
  managed <- gets stManaged
  let uses = mapUses env managed (usedVarsStmt stmt)
  case stmt of
    SAssign x (RCall "hyd_array_own" [AVar y])
      | x == y,
        x `S.member` managed -> do
          -- Consumes the old value and defines the new one in place.
          rels <- releaseVars env (if x `S.member` liveOut then S.empty else S.singleton x)
          pure (stmt : rels, S.insert x liveOut)
    SAssign x rhs
      | x `S.member` managed ->
          if x `S.member` usedVarsRHS rhs
            then do
              -- The right-hand side reads the old value: give it a name of
              -- its own, so the old value can be released after the read.
              st <- gets id
              case typeOf env st x of
                Just ct -> do
                  old <- freshManaged ct
                  let rhs' = substRHS (M.singleton x (AVar old)) rhs
                  goBlock env liveOut [SAssign old (RAtom (AVar x)), SAssign x rhs']
                Nothing -> plain managed uses
            else defineManaged env liveOut managed uses x rhs
    SReturn (AVar v) -> do
      st <- gets id
      case typeOf env st v of
        Just ct
          | hasArrayLeaves ct,
            v `S.notMember` managed -> do
              r <- fresh ct
              ss <- retainInto r ct (AVar v)
              pure (ss ++ [SReturn (AVar r)], uses)
        _ -> pure ([stmt], uses)
    SReturn _ -> pure ([stmt], uses)
    SArrayWrite arr ix (AVar v) -> do
      st <- gets id
      case typeOf env st v of
        Just ct
          | hasArrayLeaves ct ->
              -- The element owns a reference: move one in when v dies
              -- here, and retain otherwise.
              if v `S.member` managed && v `S.notMember` liveOut
                then do
                  rels <- releaseVars env ((S.delete v uses `S.difference` liveOut) `S.intersection` managed)
                  pure (stmt : rels, liveOut <> uses)
                else do
                  r <- fresh ct
                  ss <- retainInto r ct (AVar v)
                  rels <- releaseVars env ((uses `S.difference` liveOut) `S.intersection` managed)
                  pure (ss ++ SArrayWrite arr ix (AVar r) : rels, liveOut <> uses)
        _ -> plain managed uses
    SBreak -> pure ([stmt], envBreakLive env)
    SLoop spec body
      | isParallelLoop spec -> borrow managed uses
      | not (touchesManaged managed body),
        not (storesArrays (envTypes env) body) -> borrow managed uses
      | otherwise -> serialLoop spec body
    SParallelRegion {} -> borrow managed uses
    SIf c thn els -> do
      (thn', liveThn) <- goBlock env (if endsInReturn thn then S.empty else liveOut) thn
      (els', liveEls) <- goBlock env (if endsInReturn els then S.empty else liveOut) els
      managed' <- gets stManaged
      let liveIn = mapUses env managed' (usedVarsAtom c) <> liveThn <> liveEls
      relThn <- releaseVars env ((liveIn `S.difference` liveThn) `S.intersection` managed')
      relEls <- releaseVars env ((liveIn `S.difference` liveEls) `S.intersection` managed')
      pure ([SIf c (relThn ++ thn') (relEls ++ els')], liveIn)
    _ -> plain managed uses
  where
    plain managed uses = do
      rels <- releaseVars env ((uses `S.difference` liveOut) `S.intersection` managed)
      pure (stmt : rels, liveOut <> uses)

    -- A loop that assigns no managed variable borrows what it reads.
    borrow managed uses = do
      rels <- releaseVars env ((uses `S.difference` liveOut) `S.intersection` managed)
      pure (stmt : rels, liveOut <> uses)

    touchesManaged managed body =
      not (S.null (allAssigned body `S.intersection` managed))

    serialLoop spec body = do
      managed <- gets stManaged
      let specUses =
            mapUses env managed $
              foldMap usedVarsIndexExpr (lsBounds spec ++ lsOrigins spec ++ maybe [] (pure . rsInit) (lsRed spec))
          entryLive = liveOut <> specUses
      st <- gets id
      -- The header's live set: what is live after the loop, plus what the
      -- body reads before writing when its end flows back to the header.
      let bodyLiveIn h = snd (fst (runState (goBlock env {envBreakLive = h} h body) st))
          header h =
            let h' = entryLive <> bodyLiveIn h
             in if h' == h then h else header h'
          hdr = header entryLive
      (body', liveBody) <- goBlock env {envBreakLive = hdr} hdr body
      managed' <- gets stManaged
      entryRels <- releaseVars env ((hdr `S.difference` liveBody) `S.intersection` managed')
      exitRels <- releaseVars env ((hdr `S.difference` liveOut) `S.intersection` managed')
      pure (SLoop spec (entryRels ++ body') : exitRels, hdr)

-- | Define managed @x@: move or retain what the right-hand side takes from
-- other variables, then release what dies here (including @x@, if unused).
defineManaged :: Env -> Set CVar -> Set CVar -> Set CVar -> CVar -> RHS -> M ([Stmt], Set CVar)
defineManaged env liveOut managed uses x rhs = do
  st <- gets id
  let tyOf = typeOf env st
      ct = maybe (error "Ownership: managed variable without a type") id (tyOf x)
      owned v = v `S.member` managed
      dying v = owned v && v `S.notMember` liveOut
  (defStmts, consumed) <- case rhs of
    RAtom a@(AVar v)
      | dying v -> pure ([SAssign x rhs], S.singleton v)
      | otherwise -> do
          ss <- retainInto x ct a
          pure (ss, S.empty)
    RCall "hyd_array_own" [a@(AVar v)]
      | dying v -> pure ([SAssign x rhs], S.singleton v)
      | otherwise -> do
          r <- fresh ct
          ss <- retainInto r ct a
          pure (ss ++ [SAssign x (RCall "hyd_array_own" [AVar r])], S.empty)
    _
      | Just (p, sel) <- projection tyOf rhs,
        Just takenSels <- M.lookup p (envDestructured env) ->
          -- Take the component; the aggregate's last projection releases
          -- what no projection takes.
          if dying p
            then do
              let others = [(t, proj) | (s, t, proj) <- maybe [] components (tyOf p), s /= sel, s `S.notMember` takenSels, hasArrayLeaves t]
              rels <- fmap concat $ forM others $ \(t, proj) -> do
                q <- fresh t
                rest <- releaseAtom t (AVar q)
                pure (SAssign q (proj (AVar p)) : rest)
              pure (SAssign x rhs : rels, S.singleton p)
            else pure ([SAssign x rhs], S.empty)
      | Just (p, sel) <- projection tyOf rhs ->
          if dying p
            then do
              let others = [(t, proj) | (s, t, proj) <- maybe [] components (tyOf p), s /= sel, hasArrayLeaves t]
              rels <- fmap concat $ forM others $ \(t, proj) -> do
                q <- fresh t
                rest <- releaseAtom t (AVar q)
                pure (SAssign q (proj (AVar p)) : rest)
              pure (SAssign x rhs : rels, S.singleton p)
            else do
              t <- fresh ct
              ss <- retainInto x ct (AVar t)
              pure (SAssign t rhs : ss, S.empty)
    RArrayLoad {} -> do
      -- An element is borrowed from the array that holds it.
      t <- fresh ct
      ss <- retainInto x ct (AVar t)
      pure (SAssign t rhs : ss, S.empty)
    RPairMake e1 e2 a1 a2 -> do
      (pre, atoms', moved) <- takeComponents tyOf dying (map snd (componentTypes ct)) [a1, a2]
      case atoms' of
        [a1', a2'] -> pure (pre ++ [SAssign x (RPairMake e1 e2 a1' a2')], moved)
        _ -> error "Ownership: a pair has two components"
    RRecord fields -> do
      let fieldTy f = case ct of
            CTRecord fs -> maybe CTUnknown id (lookup f fs)
            _ -> CTUnknown
      (pre, atoms', moved) <- takeComponents tyOf dying (map (fieldTy . fst) fields) (map snd fields)
      pure (pre ++ [SAssign x (RRecord (zip (map fst fields) atoms'))], moved)
    _ -> pure ([SAssign x rhs], S.empty)
  let dead = ((uses `S.difference` consumed) `S.difference` liveOut) `S.intersection` managed
  rels <- releaseVars env (if x `S.member` liveOut then dead else S.insert x dead)
  pure (defStmts ++ rels, S.delete x liveOut <> uses)

componentTypes :: CType -> [(Sel, CType)]
componentTypes ct = [(s, t) | (s, t, _) <- components ct]

-- | Move each dying managed component into the aggregate; retain the rest.
-- A variable used for two components moves into at most one of them.
takeComponents :: (CVar -> Maybe CType) -> (CVar -> Bool) -> [CType] -> [Atom] -> M ([Stmt], [Atom], Set CVar)
takeComponents tyOf dying compTys atoms = go S.empty (zip compTys atoms)
  where
    go moved [] = pure ([], [], moved)
    go moved ((t, a) : rest) = case a of
      AVar v
        | hasArrayLeaves t,
          isJust (tyOf v) -> do
            (pre, a', moved') <-
              if dying v && v `S.notMember` moved
                then pure ([], a, S.insert v moved)
                else do
                  r <- fresh t
                  ss <- retainInto r t a
                  pure (ss, AVar r, moved)
            (pre', atoms', moved'') <- go moved' rest
            pure (pre ++ pre', a' : atoms', moved'')
      _ -> do
        (pre', atoms', moved') <- go moved rest
        pure (pre', a : atoms', moved')

-- | A projection of a component out of an aggregate variable.
projection :: (CVar -> Maybe CType) -> RHS -> Maybe (CVar, Sel)
projection tyOf rhs = case rhs of
  RPairFst _ (AVar p) -> Just (p, SelFst)
  RPairSnd _ (AVar p) -> Just (p, SelSnd)
  RProj i (AVar p)
    | Just (CTPair _ _) <- tyOf p -> Just (p, if i == 0 then SelFst else SelSnd)
  RRecordProj f (AVar p) -> Just (p, SelField f)
  _ -> Nothing
