{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CodegenCSpec (spec) where

import Data.List (isInfixOf)
import Data.Map qualified as Map
import Language.Hydrangea.TestUtil (containsAll, containsNone, containsInOrder)
import Language.Hydrangea.CFGCore (Atom(..))
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.CFG
import Language.Hydrangea.CodegenC
  ( BenchmarkConfig(..)
  , CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram
  , codegenProgramWithOptions
  , codegenProgramWithOptionsPrune
  , defaultCodegenOptions
  )
import Test.Hspec

spec :: Spec
spec = describe "CodegenC" $ do
  it "emits parallel pragma from Parallel ExecPolicy" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelGeneric Nothing Nothing)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for"

  it "emits omp simd from Vector ExecPolicy" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Vector (VectorSpec 4 TailRemainder)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(4)"

  it "omits omp simd for very short vector loops" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 3] (Vector (VectorSpec 4 TailRemainder)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` (not . isInfixOf "#pragma omp simd simdlen(4)")

  it "emits parallel reduction clause from ReductionSpec" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        spec' = LoopSpec ["i"] [IConst 16] (Parallel (ParallelSpec ParallelGeneric Nothing Nothing))
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction []
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AInt 0)]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "reduction(+:acc)"

  it "emits collapse for ND parallel loops" $ do
    let spec' = LoopSpec ["i", "j", "k"] [IConst 2, IConst 3, IConst 4]
                  (Parallel (ParallelSpec ParallelGeneric Nothing Nothing)) Nothing LoopPlain []
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for collapse(3)"

  it "emits policy clauses carried by ParallelSpec" $ do
    let spec' = LoopSpec ["i"] [IConst 8]
                  (Parallel (ParallelSpec ParallelGeneric (Just "schedule(static)") Nothing)) Nothing LoopPlain []
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for schedule(static)"

  it "emits omp parallel regions with inner worksharing loops" $ do
    let stageSpec =
          LoopSpec ["i"] [IConst 8]
            (Workshare (ParallelSpec ParallelGeneric (Just "schedule(static)") Nothing))
            Nothing
            LoopMap
            []
        prog = Program
          [ mkProc "p" []
              [ SParallelRegion
                  [ SLoop stageSpec []
                  ]
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel"
    c `shouldSatisfy` isInfixOf "#pragma omp for schedule(static)"
    c `shouldSatisfy` (not . isInfixOf "#pragma omp parallel for schedule(static)")

  it "emits role-aware comments for outer map-reduction loops" $ do
    let spec' = LoopSpec ["j"] [IConst 8] (Parallel (ParallelSpec ParallelGeneric Nothing Nothing)) Nothing LoopMapReduction []
        prog = Program [mkProc "p" [] [SLoop spec' [], SReturn (C.AInt 0)]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "map-reduction outer loop"

  it "emits ND reduction pragmas with collapse" $ do
    let body = [SAssign "acc" (C.RBinOp C.CMul (C.AVar "acc") (C.AInt 2))]
        spec' = LoopSpec ["i", "j"] [IConst 3, IConst 4]
                  (Parallel (ParallelSpec ParallelGeneric Nothing Nothing))
                  (Just (ReductionSpec "acc" (IConst 1) C.RMul)) LoopReduction []
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp parallel for collapse(2) reduction(*:acc)"

  it "emits strategy comments for direct scatter parallel loops" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterDirect Nothing Nothing)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
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
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing Nothing)) Nothing LoopPlain []) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      [ "#pragma omp parallel for /* scatter-atomic-add-int */"
      , "#pragma omp atomic update"
      , "+= val;"
      ]

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
              , SLoop (LoopSpec ["i"] [IConst 64] (Parallel (ParallelSpec ParallelScatterPrivatizedIntAdd Nothing Nothing)) Nothing LoopPlain []) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64), ("routes", C.CTArray C.CTInt64), ("vals", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      [ "#pragma omp parallel /* scatter-privatized-int-add */"
      , "#pragma omp for"
      , "calloc((size_t)"
      -- Per-thread grids preallocated outside the parallel region.
      , "omp_get_max_threads()"
      , "omp_get_thread_num()"
      , "omp_get_num_threads()"
      ]
    -- Parallel merge over output cells: no atomics, no critical section.
    c `shouldSatisfy` containsNone ["#pragma omp atomic", "#pragma omp critical"]

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
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing Nothing)) Nothing LoopPlain []) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTInt64)] }
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      [ "#pragma omp parallel for /* scatter-atomic-add-int */"
      , "if (guard)"
      , "#pragma omp atomic update"
      ]

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
              , SLoop (LoopSpec ["i"] [IConst 8] (Parallel (ParallelSpec ParallelScatterAtomicAddFloat Nothing Nothing)) Nothing LoopPlain []) loopBody
              , SReturn (C.AInt 0)
              ])
              { procTypeEnv = Map.fromList [("out", C.CTArray C.CTDouble)] }
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      [ "#pragma omp parallel for /* scatter-atomic-add-float */"
      , "#pragma omp atomic update"
      , "+= val;"
      ]

  it "emits serial 1D loop with correct bounds" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain [])
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 10LL; i++)"
    c `shouldSatisfy` isInfixOf "return"

  it "emits 2D nested loops from multiple iterators" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i","j"] [IConst 3, IConst 4] Serial Nothing LoopPlain [])
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsInOrder
      ["for (int64_t i = 0; i < 3LL; i++)", "for (int64_t j = 0; j < 4LL; j++)"]

  it "emits array write inside loop" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "arr" (C.RArrayAlloc (C.AInt 100))
              , SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain [])
                  [ SArrayWrite (AVar "arr") (AVar "i") (AInt 42) ]
              , SReturn (AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` (\out -> "->data[" `isInfixOf` out || "(((" `isInfixOf` out)
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < 10LL; i++)"

  it "emits tuple construction from RTuple" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "t" (C.RTuple [C.AInt 1, C.AInt 2])
              , SReturn (C.AVar "t")
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "(hyd_tuple_t){.ndims="

  it "emits array shape operation" $ do
    let prog = Program
          [ mkProc "p" ["arr"]
              [ SAssign "shp" (C.RArrayShape (C.AVar "arr"))
              , SReturn (C.AVar "shp")
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "->shape"

  it "emits array load" $ do
    let prog = Program
          [ mkProc "p" ["arr"]
              [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AInt 0))
              , SReturn (C.AVar "x")
              ]
          ]
        c = codegenProgram prog
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
        c = codegenProgram prog
    c `shouldSatisfy` containsAll ["int64_t x;", "double y;", ".x = 1LL", "r.x"]

  it "emits serial reduction from ReductionSpec" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        spec' = LoopSpec ["i"] [IConst 16] Serial
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction []
        prog = Program [mkProc "p" [] [SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      ["int64_t acc = 0LL;", "for (int64_t i = 0; i < 16LL; i++)"]

  it "emits variable-bound loop from IVar" $ do
    let prog = Program
          [ mkProc "p" ["n"]
              [ SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
                  [ SReturn (C.AInt 0) ]
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "for (int64_t i = 0; i < n; i++)"

  -- lsBounds are trip counts: a loop with a nonzero origin runs
  -- [origin, origin + bound) on every emission path (buglog Issue 1).
  it "emits [origin, origin+bound) for loops with nonzero origins" $ do
    let origin = IMul (IConst 2) (IVar "t")
        loopWith exec =
          Program
            [ mkProc "p" ["t"]
                [ SLoop (LoopSpec ["i"] [IConst 10] exec Nothing LoopPlain [origin])
                    [ SAssign "x" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1)) ]
                , SReturn (C.AInt 0)
                ]
            ]
        serialC = codegenProgram (loopWith Serial)
        parallelC = codegenProgram (loopWith (Parallel (ParallelSpec ParallelGeneric Nothing Nothing)))
        vectorC = codegenProgram (loopWith (Vector (VectorSpec 4 TailRemainder)))
    serialC `shouldSatisfy` containsAll ["for (int64_t i = (t * 2LL); i < ((t * 2LL) + 10LL); i++)"]
    parallelC `shouldSatisfy` containsAll ["for (int64_t i = (t * 2LL); i < ((t * 2LL) + 10LL); i++)"]
    vectorC `shouldSatisfy` containsAll ["for (int64_t i = (t * 2LL); i < ((t * 2LL) + 10LL); i++)"]

  it "emits vector loop without remainder when TailNone" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 8] (Vector (VectorSpec 4 TailNone)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      ["vectorized loop, width = 4", "#pragma omp simd simdlen(4)"]

  it "emits vector loop with remainder when TailRemainder" $ do
    let prog = Program
          [ mkProc "p" []
              [ SLoop (LoopSpec ["i"] [IConst 6] (Vector (VectorSpec 4 TailRemainder)) Nothing LoopPlain []) []
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` containsAll
      ["compiler handles tail", "#pragma omp simd simdlen(4)"]

  it "emits simd reduction clauses for vectorized reduction loops" $ do
    let body = [SAssign "acc" (C.RBinOp C.CAddF (C.AVar "acc") (C.AVar "x"))]
        spec' = LoopSpec ["i"] [IConst 16] (Vector (VectorSpec 2 TailNone))
                  (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction []
        prog = Program [mkProc "p" [] [SAssign "x" (C.RAtom (C.AFloat 1.0)), SLoop spec' body, SReturn (C.AVar "acc")]]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "#pragma omp simd simdlen(2) reduction(+:acc)"

  it "emits vector intrinsics for explicit RVec lowering" $ do
    let prog = Program
          [ mkProc "p" []
              [ SAssign "shape" (C.RTuple [C.AInt 8])
              , SAssign "arr" (C.RArrayAlloc (C.AVar "shape"))
              , SAssign "out" (C.RArrayAlloc (C.AVar "shape"))
              , SAssign "n" (C.RAtom (C.AInt 8))
              , SAssign "i__vec_trips" (C.RBinOp C.CDiv (C.AVar "n") (C.AInt 4))
              , SLoop (LoopSpec ["i__vec_i"] [IVar "i__vec_trips"] Serial Nothing LoopPlain [])
                  [ SAssign "i__vec_base" (C.RBinOp C.CMul (C.AVar "i__vec_i") (C.AInt 4))
                  , SAssign "x__vec" (C.RVecLoad (C.AVar "arr") (C.AVar "i__vec_base"))
                  , SAssign "bias__vec" (C.RVecSplat (C.AFloat 1.0))
                  , SAssign "y__vec" (C.RVecBinOp C.CAddF (C.AVecVar "x__vec") (C.AVecVar "bias__vec"))
                  , SAssign "__vec_store_discard" (C.RVecStore (C.AVar "out") (C.AVar "i__vec_base") (C.AVecVar "y__vec"))
                  ]
              , SReturn (C.AInt 0)
              ]
          ]
        c = codegenProgram prog
    c `shouldSatisfy` isInfixOf "hyd_float64x4_t x__vec"
    -- The lowering must emit load, then add, then store, in that order.
    c `shouldSatisfy` containsInOrder
      ["hyd_vec_loadu_f64x4", "hyd_vec_add_f64x4", "hyd_vec_storeu_f64x4"]

  it "disables memoization for pure zero-arg array procs in benchmark closure" $ do
    let inputProc =
          (mkProc "input_data" []
             [ SAssign "arr" (C.RArrayAlloc (C.AInt 8))
             , SReturn (C.AVar "arr")
             ])
            { procTypeEnv = Map.fromList [("arr", C.CTArray C.CTInt64)] }
        kernelProc =
          (mkProc "kernel" []
             [ SAssign "tmp" (C.RCall "input_data" [])
             , SReturn (C.AVar "tmp")
             ])
            { procTypeEnv = Map.fromList [("tmp", C.CTArray C.CTInt64)] }
        prog = Program [inputProc, kernelProc]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenBenchmark = Just (BenchmarkConfig "kernel" 1 1)
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } ->
        c `shouldNotSatisfy` isInfixOf "static hyd_array_t* __cache_input_data = NULL;"

  it "keeps memoization for benchmark-closure zero-arg array procs that perform IO" $ do
    let inputProc =
          (mkProc "input_data" []
             [ SAssign "arr" (C.RCall "hyd_read_array_csv" [C.AString "input.csv"])
             , SReturn (C.AVar "arr")
             ])
            { procTypeEnv = Map.fromList [("arr", C.CTArray C.CTInt64)] }
        kernelProc =
          (mkProc "kernel" []
             [ SAssign "tmp" (C.RCall "input_data" [])
             , SReturn (C.AVar "tmp")
             ])
            { procTypeEnv = Map.fromList [("tmp", C.CTArray C.CTInt64)] }
        prog = Program [inputProc, kernelProc]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenBenchmark = Just (BenchmarkConfig "kernel" 1 1)
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } ->
        c `shouldSatisfy` isInfixOf "static hyd_array_t* __cache_input_data = NULL;"

  it "strips benchmark IO writes from benchmark kernel body only" $ do
    let kernelProc =
          (mkProc "kernel" []
             [ SAssign "arr" (C.RArrayAlloc (C.AInt 8))
             , SAssign "_" (C.RCall "hyd_write_array_csv" [C.AString "out.csv", C.AVar "arr"])
             , SReturn (C.AVar "arr")
             ])
            { procTypeEnv = Map.fromList [("arr", C.CTArray C.CTInt64)] }
        prog = Program [kernelProc]
        nonBenchArtifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              }
            prog
        benchArtifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenBenchmark = Just (BenchmarkConfig "kernel" 1 1)
              }
            prog
    case nonBenchArtifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } ->
        c `shouldSatisfy` isInfixOf "hyd_write_array_csv("
    case benchArtifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } ->
        c `shouldNotSatisfy` isInfixOf "hyd_write_array_csv("

  it "resets benchmark kernel cache before warmup and timed iterations" $ do
    let kernelProc =
          (mkProc "kernel" []
             [ SAssign "arr" (C.RArrayAlloc (C.AInt 8))
             , SReturn (C.AVar "arr")
             ])
            { procTypeEnv = Map.fromList [("arr", C.CTArray C.CTInt64)] }
        prog = Program [kernelProc]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenBenchmark = Just (BenchmarkConfig "kernel" 2 3)
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } ->
        c `shouldSatisfy` isInfixOf "__bench_cache_kernel = NULL;"

  it "benchmark mode main only runs the selected benchmark kernel" $ do
    let helperProc = mkProc "helper" [] [SReturn (C.AInt 7)]
        kernelProc = mkProc "kernel" [] [SReturn (C.AFloat 1.0)]
        prog = Program [helperProc, kernelProc]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenBenchmark = Just (BenchmarkConfig "kernel" 1 2)
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } -> do
        c `shouldNotSatisfy` isInfixOf "helper_result"
        c `shouldSatisfy` isInfixOf "benchmark[kernel]"

  it "emits export wrapper and suppresses main in export mode" $ do
    let prog = Program [mkProc "main" [] [SReturn (C.AInt 42)]]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just "main"
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c, codegenHeader = mHeader } -> do
        c `shouldSatisfy` containsAll ["int64_t hyd_export_main(void)", "return hyd_main();"]
        c `shouldNotSatisfy` isInfixOf "int main(void)"
        mHeader `shouldSatisfy` maybe False (isInfixOf "int64_t hyd_export_main(void);")

  it "exports a parameterized kernel with forwarded arguments" $ do
    let prog = Program [mkProc "kernel" ["n"] [SReturn (C.AVar "n")]]
        artifacts =
          codegenProgramWithOptions
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just "kernel"
              }
            prog
    case artifacts of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c, codegenHeader = mHeader } -> do
        c `shouldSatisfy` containsAll ["int64_t hyd_export_kernel(int64_t n)", "return kernel(n);"]
        mHeader `shouldSatisfy` maybe False (isInfixOf "int64_t hyd_export_kernel(int64_t n);")

  it "prunes uncalled zero-arg procs when pruning is enabled" $ do
    let prog =
          Program
            [ mkProc "main" [] [SReturn (C.AInt 42)]
            , mkProc "dead" [] [SReturn (C.AInt 7)]
            ]
    case codegenProgramWithOptionsPrune defaultCodegenOptions True prog of
      Left err -> expectationFailure err
      Right CodegenArtifacts { codegenSource = c } -> do
        c `shouldSatisfy` isInfixOf "int64_t hyd_main(void)"
        c `shouldNotSatisfy` isInfixOf "dead(void)"
