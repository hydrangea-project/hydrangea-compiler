{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CodegenCSpec (spec) where

import Data.List (isInfixOf)
import Data.Map qualified as Map
import Language.Hydrangea.CFGCore (Atom(..))
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.CFG
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram2
  , codegenProgram2WithOptions
  , defaultCodegenOptions
  )
import Test.Hspec

spec :: Spec
spec = describe "CodegenC" $ do
  it "emits parallel pragma from Parallel ExecPolicy" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelGeneric Nothing)) Nothing LoopPlain) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for"

  it "emits omp simd from Vector ExecPolicy" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Vector (VectorSpec 4 TailRemainder)) Nothing LoopPlain) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(4)"

  it "emits parallel reduction clause from ReductionSpec" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        spec' = LoopSpec ["i"] [IConst 16] (Parallel (ParallelSpec ParallelGeneric Nothing))
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AInt 0)]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "reduction(+:acc)"

  it "emits collapse for ND parallel loops" $ do
    let spec' = LoopSpec ["i", "j", "k"] [IConst 2, IConst 3, IConst 4]
                  (Parallel (ParallelSpec ParallelGeneric Nothing)) Nothing LoopPlain
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for collapse(3)"

  it "emits policy clauses carried by ParallelSpec" $ do
    let spec' = LoopSpec ["i"] [IConst 8]
                  (Parallel (ParallelSpec ParallelGeneric (Just "schedule(static)"))) Nothing LoopPlain
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for schedule(static)"

  it "emits role-aware comments for outer map-reduction loops" $ do
    let spec' = LoopSpec ["j"] [IConst 8] (Parallel (ParallelSpec ParallelGeneric Nothing)) Nothing LoopMapReduction
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "map-reduction outer loop"

  it "emits ND reduction pragmas with collapse" $ do
    let body = [SAssign "acc" (C.RBinOp C.CMul (C.AVar "acc") (C.AInt 2))]
        spec' = LoopSpec ["i", "j"] [IConst 3, IConst 4]
                  (Parallel (ParallelSpec ParallelGeneric Nothing))
                  (Just (ReductionSpec "acc" (IConst 1) C.RMul)) LoopReduction
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for collapse(2) reduction(*:acc)"

  it "emits strategy comments for direct scatter parallel loops" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterDirect Nothing)) Nothing LoopPlain) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-direct */"

  it "emits atomic update code for integer scatter-add loops" $ do
    let loopBody =
          [ SAssign "idx" (C.RAtom (C.AVar "i"))
          , SAssign "val" (C.RAtom (C.AInt 1))
          , SAssign "old" (C.RArrayLoad (C.AVar "out") (C.AVar "idx"))
          , SAssign "new" (C.RBinOp C.CAdd (C.AVar "val") (C.AVar "old"))
          , SArrayWrite (C.AVar "out") (C.AVar "idx") (C.AVar "new")
          ]
        prog = Program
          [ (mkProc "p" []
              [ SAssign "out" (C.RArrayAlloc (C.AInt 8))
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing)) Nothing LoopPlain) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-int */"
    c `shouldSatisfy` isInfixOf "#pragma omp atomic update"
    c `shouldSatisfy` isInfixOf "+= val;"

  it "emits privatized scatter code for dense integer scatter-add loops" $ do
    let loopBody =
          [ SAssign "idx" (C.RArrayLoad (C.AVar "routes") (C.AVar "i"))
          , SAssign "val" (C.RArrayLoad (C.AVar "vals") (C.AVar "i"))
          , SAssign "old" (C.RArrayLoad (C.AVar "out") (C.AVar "idx"))
          , SAssign "new" (C.RBinOp C.CAdd (C.AVar "val") (C.AVar "old"))
          , SArrayWrite (C.AVar "out") (C.AVar "idx") (C.AVar "new")
          ]
        prog = Program
          [ (mkProc "p" []
              [ SAssign "shp" (C.RTuple [C.AInt 8])
              , SAssign "out" (C.RArrayAlloc (C.AVar "shp"))
              , SLoop (LoopSpec ["i"] [IConst 64] (Parallel (ParallelSpec ParallelScatterPrivatizedIntAdd Nothing)) Nothing LoopPlain) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64), ("routes", C.CTArray C.CTInt64), ("vals", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel /* scatter-privatized-int-add */"
    c `shouldSatisfy` isInfixOf "#pragma omp for"
    c `shouldSatisfy` isInfixOf "#pragma omp critical"
    c `shouldSatisfy` isInfixOf "calloc((size_t)"

  it "emits guarded atomic update code for guarded integer scatter-add loops" $ do
    let loopBody =
          [ SAssign "guard" (C.RAtom (C.AVar "g"))
          , SIf (C.AVar "guard")
              [ SAssign "idx" (C.RAtom (C.AVar "i"))
              , SAssign "val" (C.RAtom (C.AInt 1))
              , SAssign "old" (C.RArrayLoad (C.AVar "out") (C.AVar "idx"))
              , SAssign "new" (C.RBinOp C.CAdd (C.AVar "val") (C.AVar "old"))
              , SArrayWrite (C.AVar "out") (C.AVar "idx") (C.AVar "new")
              ]
              []
          ]
        prog = Program
          [ (mkProc "p" []
              [ SAssign "out" (C.RArrayAlloc (C.AInt 8))
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing)) Nothing LoopPlain) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-int */"
    c `shouldSatisfy` isInfixOf "if (guard)"
    c `shouldSatisfy` isInfixOf "#pragma omp atomic update"

  it "emits atomic update code for floating-point scatter-add loops" $ do
    let loopBody =
          [ SAssign "idx" (C.RAtom (C.AVar "i"))
          , SAssign "val" (C.RAtom (C.AFloat 1.5))
          , SAssign "old" (C.RArrayLoad (C.AVar "out") (C.AVar "idx"))
          , SAssign "new" (C.RBinOp C.CAddF (C.AVar "val") (C.AVar "old"))
          , SArrayWrite (C.AVar "out") (C.AVar "idx") (C.AVar "new")
          ]
        prog = Program
          [ (mkProc "p" []
              [ SAssign "out" (C.RArrayAlloc (C.AInt 8))
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddFloat Nothing)) Nothing LoopPlain) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTDouble)] }
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-float */"
    c `shouldSatisfy` isInfixOf "#pragma omp atomic update"
    c `shouldSatisfy` isInfixOf "+= val;"

  it "emits serial 1D loop with correct bounds" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain)
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 10LL; i++)"
    c `shouldSatisfy` isInfixOf "return"

  it "emits 2D nested loops from multiple iterators" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i","j"] [IConst 3, IConst 4] Serial Nothing LoopPlain)
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 3LL; i++)"
    c `shouldSatisfy` isInfixOf "for (int64_t j = 0; j < 4LL; j++)"

  it "emits array write inside loop" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "arr" (C.RArrayAlloc (C.AInt 100))
              , SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain)
                  [ SArrayWrite (AVar "arr") (AVar "i") (AInt 42) ]
              , SReturn (AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` (\out -> "->data[" `isInfixOf` out || "(((" `isInfixOf` out)
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 10LL; i++)"

  it "emits tuple construction from RTuple" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "t" (C.RTuple [C.AInt 1, C.AInt 2])
              , SReturn (C.AVar "t")
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "hyd_tuple_make"

  it "emits array shape operation" $ do
    let prog = Program
          [ mkProc "p" ["arr"]
              [ SAssign "shp" (C.RArrayShape (C.AVar "arr"))
              , SReturn (C.AVar "shp")
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "->shape"

  it "emits array load" $ do
    let prog = Program
          [ mkProc "p" ["arr"]
              [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AInt 0))
              , SReturn (C.AVar "x")
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` (\out -> "->data[" `isInfixOf` out || "(((" `isInfixOf` out)

  it "emits record struct typedefs and field access from CTRecord" $ do
    let prog = Program
          [ (mkProc "p" []
                [ SAssign "r" (C.RRecord [("x", C.AInt 1), ("y", C.AFloat 2.0)])
                , SAssign "xv" (C.RRecordProj "x" (C.AVar "r"))
                , SReturn (C.AVar "xv")
                ])
              { procTypeEnv = Map.fromList [("r", C.CTRecord [("x", C.CTInt64), ("y", C.CTDouble)]), ("xv", C.CTInt64)] }
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "int64_t x;"
    c `shouldSatisfy` isInfixOf "double y;"
    c `shouldSatisfy` isInfixOf ".x = 1LL"
    c `shouldSatisfy` isInfixOf "r.x"

  it "emits serial reduction from ReductionSpec" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        spec' = LoopSpec ["i"] [IConst 16] Serial
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "int64_t acc = 0LL;"
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 16LL; i++)"

  it "emits variable-bound loop from IVar" $ do
    let prog = Program
          [ mkProc "p" ["n"]
              [ SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < n; i++)"

  it "emits vector loop without remainder when TailNone" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Vector (VectorSpec 4 TailNone)) Nothing LoopPlain) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "vectorized loop, width = 4"
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(4)"

  it "emits vector loop with remainder when TailRemainder" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 6] (Vector (VectorSpec 4 TailRemainder)) Nothing LoopPlain) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "compiler handles tail"
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(4)"

  it "emits simd reduction clauses for vectorized reduction loops" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAddF (C.AVar "acc") (C.AVar "x"))]
        spec' = LoopSpec ["i"] [IConst 16] (Vector (VectorSpec 2 TailNone))
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction
        prog = Program [mkProc "p" [] [SAssign "x" (C.RAtom (C.AFloat 1.0)), SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(2) reduction(+:acc)"

  it "emits vector intrinsics for explicit RVec lowering" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "shape" (C.RTuple [C.AInt 8])
              , SAssign "arr" (C.RArrayAlloc (C.AVar "shape"))
              , SAssign "out" (C.RArrayAlloc (C.AVar "shape"))
              , SAssign "n" (C.RAtom (C.AInt 8))
              , SAssign "i__vec_trips" (C.RBinOp C.CDiv (C.AVar "n") (C.AInt 2))
              , SLoop (LoopSpec ["i__vec_i"] [IVar "i__vec_trips"] Serial Nothing LoopPlain)
                  [ SAssign "i__vec_base" (C.RBinOp C.CMul (C.AVar "i__vec_i") (C.AInt 2))
                  , SAssign "x__vec" (C.RVecLoad (C.AVar "arr") (C.AVar "i__vec_base"))
                  , SAssign "bias__vec" (C.RVecSplat (C.AFloat 1.0))
                  , SAssign "y__vec" (C.RVecBinOp C.CAddF (C.AVecVar "x__vec") (C.AVecVar "bias__vec"))
                  , SAssign "__vec_store_discard" (C.RVecStore (C.AVar "out") (C.AVar "i__vec_base") (C.AVecVar "y__vec"))
                  ]
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram2 prog
    c `shouldSatisfy` isInfixOf "hyd_float64x2_t x__vec"
    c `shouldSatisfy` isInfixOf "hyd_vec_loadu_f64"
    c `shouldSatisfy` isInfixOf "hyd_vec_add_f64"
    c `shouldSatisfy` isInfixOf "hyd_vec_storeu_f64"

  it "emits export wrapper and suppresses main in export mode" $ do
    let prog = Program [mkProc "main" [] [SReturn (C.AInt 42)]]
        artifacts =
          codegenProgram2WithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just "main"
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c, codegenHeader = mHeader } -> do
        c `shouldSatisfy` isInfixOf "int64_t hyd_export_main(void)"
        c `shouldSatisfy` isInfixOf "return hyd_main();"
        c `shouldNotSatisfy` isInfixOf "int main(void)"
        mHeader `shouldSatisfy` maybe False (isInfixOf "int64_t hyd_export_main(void);")

  it "rejects exporting a non-zero-argument kernel" $ do
    let prog = Program [mkProc "kernel" ["n"] [SReturn (C.AVar "n")]]
        artifacts =
          codegenProgram2WithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just "kernel"
              }
            prog
    artifacts `shouldSatisfy`
      (\result -> case result of
          Left msg -> "zero-argument" `isInfixOf` msg
          Right _ -> False
      )
