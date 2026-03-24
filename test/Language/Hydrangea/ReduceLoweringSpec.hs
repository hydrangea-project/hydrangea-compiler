{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.ReduceLoweringSpec (spec) where

import Test.Hspec
import Data.ByteString.Lazy.Char8 qualified as BS

import Language.Hydrangea.Lexer hiding (Forall)
import Language.Hydrangea.Parser
import Language.Hydrangea.CFGCore (RHS(..), Atom(..), CVar, Redop(..))
import Data.Set qualified as S
import Language.Hydrangea.Lowering (lowerDecs2)
import Language.Hydrangea.Syntax
import Language.Hydrangea.CFG hiding (CVar)

-- Helper to parse and lower source code (small copy of parser helper)
parseSource :: BS.ByteString -> Either String [Dec Range]
parseSource str = runAlex str parseDecs

lowerFromSource :: BS.ByteString -> IO Program
lowerFromSource src =
  case parseSource src of
    Left err -> fail $ "Parse error: " ++ err
    Right decs -> pure $ lowerDecs2 decs

spec :: Spec
spec = describe "Lowering - Reduce regression" $ do
  it "does not produce self-referential temporaries when inlining reductions" $ do
    -- This source defines a simple binary add and reduces a generated array.
    -- Regression previously produced declarations like `t28 = (t28 + elem)`
    -- where a variable was initialized using itself. We assert no statement
    -- is the first appearance of a variable while its RHS refers to itself.
    let src = BS.pack "let sum = let add x y = x + y in reduce add 0 (fill [3] 1)"
    Program procs <- lowerFromSource src
    procs `shouldSatisfy` (not . null)
    let Proc { procBody = stmts } = case procs of
          (proc : _) -> proc
          [] -> error "expected at least one proc"

    let checkStmts = go S.empty
          where
            go _ [] = True
            go assigned (s:ss) =
              case s of
                SAssign v rhs ->
                  let rhsVars = rhsAtoms rhs
                      selfRef = v `S.member` rhsVars
                      isFirst = not (v `S.member` assigned)
                      assigned' = S.insert v assigned
                  in if selfRef && isFirst then False else go assigned' ss
                SArrayWrite{} -> go assigned ss
                SLoop _ body -> go assigned body && go assigned ss
                SIf _ t e -> go assigned t && go assigned e && go assigned ss
                SReturn _ -> go assigned ss

    checkStmts stmts `shouldBe` True

  it "preserves multiplication reductions as RMul metadata" $ do
    let src = BS.pack "let prod = let mul x y = x * y in reduce mul 1 (fill [3] 2)"
    Program procs <- lowerFromSource src
    procs `shouldSatisfy` (not . null)
    let Proc { procBody = stmts } = case procs of
          (proc : _) -> proc
          [] -> error "expected at least one proc"
    reductionOps stmts `shouldContain` [RMul]

  it "does not mark non-associative subtraction as a reduction" $ do
    let src = BS.pack "let suball = let sub x y = x - y in reduce sub 0 (fill [3] 1)"
    Program procs <- lowerFromSource src
    procs `shouldSatisfy` (not . null)
    let Proc { procBody = stmts } = case procs of
          (proc : _) -> proc
          [] -> error "expected at least one proc"
    reductionOps stmts `shouldBe` []

  it "marks reduce kernels as outer map-reduction plus inner reduction loops" $ do
    let src = BS.pack "let sum = let add x y = x + y in reduce add 0 (fill [3] 1)"
    Program procs <- lowerFromSource src
    procs `shouldSatisfy` (not . null)
    let Proc { procBody = stmts } = case procs of
          (proc : _) -> proc
          [] -> error "expected at least one proc"
    take 2 (filter (`elem` [LoopMapReduction, LoopReduction]) (loopRoles stmts))
      `shouldBe` [LoopMapReduction, LoopReduction]

  it "marks scalarized reduce_generate with a wrapper loop plus inner reduction" $ do
    let src = BS.pack $ unlines
          [ "let K = 3"
          , "let add x y = x +. y"
          , "let gen [idx] = 1.0"
          , "let scalar = index () (reduce_generate add 0.0 [K] gen)"
          ]
    Program procs <- lowerFromSource src
    procs `shouldSatisfy` (not . null)
    let Proc { procBody = stmts } = case filter ((== "scalar") . procName) procs of
          (proc : _) -> proc
          [] -> error "expected scalar proc"
    take 2 (filter (`elem` [LoopReductionWrapper, LoopReduction]) (loopRoles stmts))
      `shouldBe` [LoopReductionWrapper, LoopReduction]

-- Collect variable atoms appearing in an RHS
rhsAtoms :: RHS -> S.Set CVar
rhsAtoms rhs =
  case rhs of
    RAtom a -> atomVars a
    RBinOp _ a b -> S.union (atomVars a) (atomVars b)
    RUnOp _ a -> atomVars a
    RTuple as -> S.unions (map atomVars as)
    RProj _ a -> atomVars a
    RArrayAlloc a -> atomVars a
    RArrayLoad a b -> S.union (atomVars a) (atomVars b)
    RArrayShape a -> atomVars a
    RShapeSize a -> atomVars a
    RShapeInit a -> atomVars a
    RShapeLast a -> atomVars a
    RFlatToNd a b -> S.union (atomVars a) (atomVars b)
    RNdToFlat a b -> S.union (atomVars a) (atomVars b)
    R2DToFlat a b -> S.union (atomVars a) (atomVars b)
    RCall _ args -> S.unions (map atomVars args)
    RVecLoad a b -> S.union (atomVars a) (atomVars b)
    RVecStore a b c -> S.unions (map atomVars [a,b,c])
    RVecBinOp _ a b -> S.union (atomVars a) (atomVars b)
    RVecSplat a -> atomVars a
    RVecReduce _ a -> atomVars a
    _ -> S.empty

atomVars :: Atom -> S.Set CVar
atomVars a =
  case a of
    AVar v -> S.singleton v
    AVecVar v -> S.singleton v
    _ -> S.empty

reductionOps :: [Stmt] -> [Redop]
reductionOps = concatMap go
  where
    go stmt = case stmt of
      SLoop loopSpec body -> maybe id (:) (rsRedop <$> lsRed loopSpec) (reductionOps body)
      SIf _ thn els -> reductionOps thn ++ reductionOps els
      _ -> []

loopRoles :: [Stmt] -> [LoopRole]
loopRoles = concatMap go
  where
    go stmt = case stmt of
      SLoop loopSpec body -> lsRole loopSpec : loopRoles body
      SIf _ thn els -> loopRoles thn ++ loopRoles els
      _ -> []
