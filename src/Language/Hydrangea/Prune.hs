-- |
-- Module: Language.Hydrangea.Prune
--
-- Helpers for pruning top-level declarations and checking whether fusion
-- absorbed supporting bindings into a kernel.
module Language.Hydrangea.Prune (pruneDecsToMain, checkKernelFused) where

import Language.Hydrangea.Syntax
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Fusion (fuseDecs)
import Language.Hydrangea.ShapeNormalize (normalizeShapesDecs)
import Language.Hydrangea.Uniquify (uniquifyDecs)
import Data.ByteString.Lazy.Char8 qualified as BS
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.List (intercalate)

-- | Collect variables bound by a pattern.
patVars :: Pat a -> S.Set Var
patVars (PVar _ v) = S.singleton v
patVars (PVec _ ps) = S.unions (map patVars ps)

-- | Collect free references to names in @topNames@ within @expr@, respecting
-- local shadowing.  Variables in @bound@ are treated as locally bound and
-- are not reported even if they appear in @topNames@.
collectTopLevelRefs :: S.Set Var -> Exp Range -> S.Set Var -> S.Set Var
collectTopLevelRefs topNames expr bound =
  case expr of
    EVar _ v
      | v `S.member` topNames && not (v `S.member` bound) -> S.singleton v
    EVar{} -> S.empty

    ELetIn _ (Dec _ name pats _ decBody) body ->
      let patBound = S.unions (map patVars pats)
       in go decBody patBound `S.union` go body (S.insert name (patBound `S.union` bound))

    EVec _ es -> S.unions (map (`go` bound) es)
    EApp _ f x -> go f bound `S.union` go x bound
    EIfThen _ c t -> go c bound `S.union` go t bound
    EIfThenElse _ c t f -> go c bound `S.union` go t bound `S.union` go f bound
    ENeg _ e -> go e bound
    EBinOp _ l _ r -> go l bound `S.union` go r bound
    EUnOp _ _ e -> go e bound
    EProj _ _ e -> go e bound
    EPair _ e1 e2 -> go e1 bound `S.union` go e2 bound
    ERecord _ fields -> S.unions [go fieldExp bound | (_, fieldExp) <- fields]
    ERecordProj _ e _ -> go e bound
    EGenerate _ s g -> go s bound `S.union` go g bound
    EMap _ f a -> go f bound `S.union` go a bound
    EZipWith _ f a1 a2 -> go f bound `S.union` go a1 bound `S.union` go a2 bound
    EReduce _ f z a -> go f bound `S.union` go z bound `S.union` go a bound
    EReduceGenerate _ f z s g -> go f bound `S.union` go z bound `S.union` go s bound `S.union` go g bound
    EFoldl _ f z a -> go f bound `S.union` go z bound `S.union` go a bound
    EScan _ f z a -> go f bound `S.union` go z bound `S.union` go a bound
    ESegmentedReduce _ f z offsets vals ->
      go f bound `S.union` go z bound `S.union` go offsets bound `S.union` go vals bound
    ESortIndices _ a -> go a bound
    EIota _ n -> go n bound
    EMakeIndex _ n a -> go n bound `S.union` go a bound
    ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
      S.unions (map (`go` bound) [nrows, ncols, nnz, rows, cols, vals])
    ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
      S.unions (map (`go` bound) [nrows, ncols, nnz, rows, cols, vals])
    EPermute _ c d p a -> S.unions (map (`go` bound) [c,d,p,a])
    EScatter _ c d idx v -> S.unions (map (`go` bound) [c,d,idx,v])
    EScatterGuarded _ c d idx v g -> S.unions (map (`go` bound) [c,d,idx,v,g])
    EScatterGenerate _ c d idx f -> S.unions (map (`go` bound) [c,d,idx,f])
    EGather _ idx a -> go idx bound `S.union` go a bound
    EIndex _ i a -> go i bound `S.union` go a bound
    ECheckIndex _ i def a -> go i bound `S.union` go def bound `S.union` go a bound
    EFill _ s v -> go s bound `S.union` go v bound
    EShapeOf _ a -> go a bound

    EReplicate _ dims a ->
      let collectDim d = case d of
            ShapeAll _ -> S.empty
            ShapeAny _ e -> go e bound
            ShapeDim _ e -> go e bound
       in S.unions (map collectDim dims) `S.union` go a bound

    ESlice _ dims a ->
      let collectS sd = case sd of
            SliceAll _ -> S.empty
            SliceRange _ s l -> go s bound `S.union` go l bound
       in S.unions (map collectS dims) `S.union` go a bound

    EReshape _ s a -> go s bound `S.union` go a bound
    EReadArray _ s f -> go s bound `S.union` go f bound
    EReadArrayFloat _ s f -> go s bound `S.union` go f bound
    EWriteArray _ arr f -> go arr bound `S.union` go f bound
    EWriteArrayFloat _ arr f -> go arr bound `S.union` go f bound
    EGetEnvInt _ e -> go e bound
    EGetEnvString _ e -> go e bound
    EStencil _ bnd f arr -> goBnd bnd bound `S.union` go f bound `S.union` go arr bound
    _ -> S.empty
  where
    go = collectTopLevelRefs topNames
    goBnd BClamp     _ = S.empty
    goBnd BWrap      _ = S.empty
    goBnd BMirror    _ = S.empty
    goBnd (BConst e) b = go e b

-- | Prune top-level declarations to only those reachable from the last
-- binding named "main".
pruneDecsToMain :: [Dec Range] -> Either String [Dec Range]
pruneDecsToMain ds =
  case ds of
    [] -> Right []
    _  ->
      let lastDec = last ds
          mainName = decName lastDec
          topNames = S.fromList (map decName ds)

          collectRefs :: Exp Range -> S.Set Var -> S.Set Var
          collectRefs = collectTopLevelRefs topNames

          decByName = Map.fromList [(decName d, d) | d <- ds]

          bfs :: [Var] -> S.Set Var -> S.Set Var
          bfs [] seen = seen
          bfs (x:xs) seen =
            let seen' = S.insert x seen
                refs = case Map.lookup x decByName of
                         Just (Dec _ _ pats _ body) -> collectRefs body (S.unions (map patVars pats))
                         _ -> S.empty
                new = S.toList (refs S.\\ seen')
             in bfs (xs ++ new) seen'

          reachable = bfs [mainName] S.empty
       in if mainName /= BS.pack "main"
            then Left "--main flag requires the last top-level binding to be named 'main'"
            else Right (filter (\t -> decName t `S.member` reachable) ds)

-- | Check that all top-level bindings other than the named kernel were
-- successfully fused/inlined into it.  Runs the full fusion pipeline
-- (uniquify → shape-normalize → fuse) and then verifies that the kernel's
-- fused body contains no free references to any of the other original
-- top-level binding names.
--
-- Returns @Right ()@ on success, or @Left msg@ naming the un-fused bindings.
checkKernelFused :: [Dec Range] -> BS.ByteString -> Either String ()
checkKernelFused ds kernelName =
  let originalNames = S.fromList (map decName ds)
      fused = fuseDecs (normalizeShapesDecs (uniquifyDecs ds))
  in case filter (\d -> decName d == kernelName) fused of
       [] -> Left $ "Kernel '" ++ BS.unpack kernelName ++ "' not found in top-level declarations"
       (Dec _ _ pats _ body : _) ->
         let otherOriginals = S.delete kernelName originalNames
             refs = collectTopLevelRefs otherOriginals body (S.unions (map patVars pats))
             unFused = refs  -- refs is already filtered to otherOriginals by collectTopLevelRefs
         in if S.null unFused
              then Right ()
              else Left $
                     "Kernel check failed: the following binding(s) were not fused into '"
                     ++ BS.unpack kernelName ++ "': "
                     ++ intercalate ", " (map BS.unpack (S.toList unFused))
