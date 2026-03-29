{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Tile
--
-- Automatic loop tiling for canonical CFG loops.
module Language.Hydrangea.Tile
  ( tileStmts2
  ) where

import Control.Monad.State.Strict (State, evalState, state)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis
  ( definedVarsStmts2
  , usedVarsIndexExpr
  , usedVarsStmts2
  )
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..))

data TileConfig = TileConfig
  { tcDefaultTile :: Integer
  , tcReductionTile :: Integer
  }

defaultTileConfig :: TileConfig
defaultTileConfig = TileConfig
  { tcDefaultTile = 32
  , tcReductionTile = 32
  }

data DimPlan
  = DimKeep CVar IndexExpr
  | DimTile TiledDim


data TiledDim = TiledDim
  { tdOrigIter :: CVar
  , tdOrigBound :: IndexExpr
  , tdTileSize :: Integer
  , tdTileIter :: CVar
  , tdTileStart :: CVar
  , tdTileRemain :: CVar
  , tdTileLen :: CVar
  , tdTileShort :: CVar
  , tdLocalIter :: CVar
  , tdBoundPrelude :: [Stmt]
  , tdBoundAtom :: Atom
  }

type TileM = State (Set CVar, Int)

tileStmts2 :: [Stmt] -> [Stmt]
tileStmts2 stmts = evalState (rewriteStmts2WithM 0 (+ 1) rewriteStmt stmts) (collectNames stmts, 0)

rewriteStmt :: Int -> Stmt -> TileM [Stmt]
rewriteStmt depth stmt = case stmt of
  SLoop spec body ->
    maybe [SLoop spec body] id <$> tileLoop depth spec body
  _ -> pure [stmt]

tileLoop :: Int -> LoopSpec -> [Stmt] -> TileM (Maybe [Stmt])
tileLoop depth spec body
  | not (shouldConsiderLoop depth spec body) = pure Nothing
  | otherwise = do
      plans <- buildDimPlans defaultTileConfig spec
      if any isTiledDim plans
        then Just <$> buildStripMinedLoop spec body plans
        else pure Nothing

shouldConsiderLoop :: Int -> LoopSpec -> [Stmt] -> Bool
shouldConsiderLoop depth spec body =
  lsExec spec == Serial
    && lsRole spec /= LoopReductionWrapper
    && lsRole spec /= LoopMap         -- plain map loops don't benefit from tiling
    && boundsAreIteratorIndependent spec
    && all (supportsAtomBound . simplifyIndexExpr) (lsBounds spec)
    && (depth > 0 || hasNestedLoop body || length (lsIters spec) > 1)

boundsAreIteratorIndependent :: LoopSpec -> Bool
boundsAreIteratorIndependent spec =
  let iterVars = S.fromList (lsIters spec)
  in all (S.null . (`S.intersection` iterVars) . usedVarsIndexExpr) (lsBounds spec)

hasNestedLoop :: [Stmt] -> Bool
hasNestedLoop = any go
  where
    go stmt = case stmt of
      SLoop {} -> True
      SIf _ thn els -> hasNestedLoop thn || hasNestedLoop els
      _ -> False

isTiledDim :: DimPlan -> Bool
isTiledDim DimKeep {} = False
isTiledDim DimTile {} = True

buildDimPlans :: TileConfig -> LoopSpec -> TileM [DimPlan]
buildDimPlans cfg spec = mapM buildOne (zip (lsIters spec) (lsBounds spec))
  where
    buildOne (iter, bound) =
      case chooseTileSize cfg spec bound of
        Nothing -> pure (DimKeep iter bound)
        Just tileSize -> do
          (prelude, boundAtom) <- indexExprToAtom bound
          tileIter <- freshLike iter "_tile"
          tileStart <- freshLike iter "_tile_start"
          tileRemain <- freshLike iter "_tile_remain"
          tileLen <- freshLike iter "_tile_len"
          tileShort <- freshLike iter "_tile_short"
          localIter <- freshLike iter "_tile_idx"
          pure (DimTile TiledDim
            { tdOrigIter = iter
            , tdOrigBound = bound
            , tdTileSize = tileSize
            , tdTileIter = tileIter
            , tdTileStart = tileStart
            , tdTileRemain = tileRemain
            , tdTileLen = tileLen
            , tdTileShort = tileShort
            , tdLocalIter = localIter
            , tdBoundPrelude = prelude
            , tdBoundAtom = boundAtom
            })

chooseTileSize :: TileConfig -> LoopSpec -> IndexExpr -> Maybe Integer
chooseTileSize cfg spec bound
  | usefulBound tileSize (simplifyIndexExpr bound) = Just tileSize
  | otherwise = Nothing
  where
    tileSize = case lsRole spec of
      LoopReduction -> tcReductionTile cfg
      LoopMap -> tcDefaultTile cfg
      _ -> tcDefaultTile cfg

    usefulBound n expr = case expr of
      IConst k -> k > n
      _ -> True

supportsAtomBound :: IndexExpr -> Bool
supportsAtomBound expr = case expr of
  IConst {} -> True
  IVar {} -> True
  IAdd a b -> supportsAtomBound a && supportsAtomBound b
  ISub a b -> supportsAtomBound a && supportsAtomBound b
  IMul a b -> supportsAtomBound a && supportsAtomBound b
  IDiv a b -> supportsAtomBound a && supportsAtomBound b
  _ -> False

buildStripMinedLoop :: LoopSpec -> [Stmt] -> [DimPlan] -> TileM [Stmt]
buildStripMinedLoop spec body plans = do
  let tiledDims = [td | DimTile td <- plans]
      tilePrelude = concatMap tdBoundPrelude tiledDims
      tileRole = tiledLoopRole (lsRole spec)
      outerTileSpec = LoopSpec
        { lsIters = map tdTileIter tiledDims
        , lsBounds = map (\td -> tileCountExpr (tdOrigBound td) (tdTileSize td)) tiledDims
        , lsExec = Serial
        , lsRed = Nothing
        , lsRole = tileRole
        }
      innerLocalSpec = spec
        { lsIters = map localIter plans
        , lsBounds = map localBound plans
        }
      outerSetup = concatMap setupDim tiledDims
      innerSetup = map assignOrigIter tiledDims
      innerLocalLoop = SLoop innerLocalSpec (innerSetup ++ body)
  pure (tilePrelude ++ [SLoop outerTileSpec (outerSetup ++ [innerLocalLoop])])
  where
    localIter plan = case plan of
      DimKeep iter _ -> iter
      DimTile td -> tdLocalIter td

    localBound plan = case plan of
      DimKeep _ bound -> bound
      DimTile td -> IVar (tdTileLen td)

setupDim :: TiledDim -> [Stmt]
setupDim td =
  [ SAssign (tdTileStart td) (RBinOp CMul (AVar (tdTileIter td)) (AInt (tdTileSize td))) ]
    ++ minLenSetup (tdBoundAtom td) (tdTileStart td) (tdTileRemain td) (tdTileShort td) (tdTileLen td) (tdTileSize td)

assignOrigIter :: TiledDim -> Stmt
assignOrigIter td =
  SAssign (tdOrigIter td) (RBinOp CAdd (AVar (tdTileStart td)) (AVar (tdLocalIter td)))

tiledLoopRole :: LoopRole -> LoopRole
tiledLoopRole role = case role of
  LoopPlain -> LoopPlain
  LoopFold -> LoopFold
  LoopMap -> LoopMap
  LoopReductionWrapper -> LoopMap
  LoopReduction -> LoopMap
  LoopMapReduction -> LoopMap

minLenSetup :: Atom -> CVar -> CVar -> CVar -> CVar -> Integer -> [Stmt]
minLenSetup boundAtom tileStart tileRemain tileShort tileLen tileSize =
  [ SAssign tileRemain (RBinOp CSub boundAtom (AVar tileStart))
  , SAssign tileShort (RBinOp CLt (AVar tileRemain) (AInt tileSize))
  , SIf
      (AVar tileShort)
      [SAssign tileLen (RAtom (AVar tileRemain))]
      [SAssign tileLen (RAtom (AInt tileSize))]
  ]

tileCountExpr :: IndexExpr -> Integer -> IndexExpr
tileCountExpr bound tileSize =
  case simplifyIndexExpr bound of
    IConst n -> IConst ((n + tileSize - 1) `div` tileSize)
    simpleBound -> simplifyIndexExpr (IDiv (IAdd simpleBound (IConst (tileSize - 1))) (IConst tileSize))

indexExprToAtom :: IndexExpr -> TileM ([Stmt], Atom)
indexExprToAtom expr = case simplifyIndexExpr expr of
  IConst n -> pure ([], AInt n)
  IVar v -> pure ([], AVar v)
  IAdd a b -> lowerBinary CAdd a b
  ISub a b -> lowerBinary CSub a b
  IMul a b -> lowerBinary CMul a b
  IDiv a b -> lowerBinary CDiv a b
  _ -> error "unsupported index expression in tiling pass"
  where
    lowerBinary op a b = do
      (sa, aa) <- indexExprToAtom a
      (sb, ab) <- indexExprToAtom b
      tmp <- freshLike "tile_bound" ""
      pure (sa ++ sb ++ [SAssign tmp (RBinOp op aa ab)], AVar tmp)

freshLike :: CVar -> ByteString -> TileM CVar
freshLike base suffix =
  state $ \(seen, nextId) ->
    let candidates = [base <> suffix <> "_" <> BS.pack (show n) | n <- [nextId ..]]
        fresh = fromMaybe (base <> suffix <> "_fresh") (listToMaybe (filter (`S.notMember` seen) candidates))
    in (fresh, (S.insert fresh seen, nextId + 1))

collectNames :: [Stmt] -> Set CVar
collectNames stmts =
  usedVarsStmts2 stmts
    `S.union` definedVarsStmts2 stmts
    `S.union` collectLoopNames stmts
  where
    collectLoopNames = foldMap go
    go stmt = case stmt of
      SLoop spec body ->
        S.fromList (lsIters spec)
          `S.union` S.unions (map usedVarsIndexExpr (lsBounds spec))
          `S.union` collectLoopNames body
      SIf _ thn els -> collectLoopNames thn `S.union` collectLoopNames els
      _ -> S.empty
