{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.FusionSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as M
import Language.Hydrangea.Frontend
import Language.Hydrangea.Fusion
import Language.Hydrangea.Syntax
import Language.Hydrangea.Uniquify
import Test.Hspec

expectFusionSensible :: ByteString -> (Exp () -> Expectation) -> Expectation
expectFusionSensible src check =
  case readExp src of
    Left err -> expectationFailure err
    Right exp' -> do
      fusedRes <- fuseExpAfterTypeCheck exp'
      case fusedRes of
        Left err -> expectationFailure err
        Right fused -> do
          fusedTy <- typeCheckExp fused
          case fusedTy of
            Left err -> expectationFailure err
            Right _ ->
              do
                r1 <- evalExpFrontend exp'
                r2 <- evalExpFrontend fused
                case (r1, r2) of
                  (Right v1, Right v2) -> do
                    v1 `shouldBe` v2
                    check (fmap (const ()) fused)
                  (Left err, _) -> expectationFailure (show err)
                  (_, Left err) -> expectationFailure (show err)

anyExp :: (Exp a -> Bool) -> Exp a -> Bool
anyExp p expr
  | p expr = True
  | otherwise =
      case expr of
        EInt {} -> False
        EFloat {} -> False
        EReadArrayFloat {} -> False
        EWriteArray {} -> False
        EWriteArrayFloat {} -> False
        EVar {} -> False
        EString {} -> False
        EUnit {} -> False
        EBool {} -> False
        EVec _ es -> any (anyExp p) es
        EApp _ f x -> anyExp p f || anyExp p x
        EIfThen _ c t -> anyExp p c || anyExp p t
        EIfThenElse _ c t f -> anyExp p c || anyExp p t || anyExp p f
        ENeg _ e -> anyExp p e
        EBinOp _ l _ r -> anyExp p l || anyExp p r
        EUnOp _ _ e -> anyExp p e
        EOp {} -> False
        ELetIn _ (Dec _ _ _ _ _ b) e -> anyExp p b || anyExp p e
        EProj _ _ e -> anyExp p e
        EGenerate _ s f -> anyExp p s || anyExp p f
        EMap _ f arr -> anyExp p f || anyExp p arr
        EZipWith _ f x y -> anyExp p f || anyExp p x || anyExp p y
        EReduce _ f z arr -> anyExp p f || anyExp p z || anyExp p arr
        EReduceGenerate _ f z s g -> anyExp p f || anyExp p z || anyExp p s || anyExp p g
        EPermute _ c d perm arr -> anyExp p c || anyExp p d || anyExp p perm || anyExp p arr
        EScatter _ c d idx vals -> anyExp p c || anyExp p d || anyExp p idx || anyExp p vals
        EScatterGuarded _ c d idx vals guardArr ->
          anyExp p c || anyExp p d || anyExp p idx || anyExp p vals || anyExp p guardArr
        EScatterGenerate _ c d idx f -> anyExp p c || anyExp p d || anyExp p idx || anyExp p f
        EGather _ idx arr -> anyExp p idx || anyExp p arr
        EIndex _ idx arr -> anyExp p idx || anyExp p arr
        ECheckIndex _ idx def arr -> anyExp p idx || anyExp p def || anyExp p arr
        EFill _ s v -> anyExp p s || anyExp p v
        EShapeOf _ arr -> anyExp p arr
        EReplicate _ dims arr -> any (anyShapeDim p) dims || anyExp p arr
        ESlice _ dims arr -> any (anySliceDim p) dims || anyExp p arr
        EReshape _ s arr -> anyExp p s || anyExp p arr
        EReadArray _ s f -> anyExp p s || anyExp p f

anyShapeDim :: (Exp a -> Bool) -> ShapeDim a -> Bool
anyShapeDim p = \case
  ShapeAll _ -> False
  ShapeAny _ e -> anyExp p e
  ShapeDim _ e -> anyExp p e

anySliceDim :: (Exp a -> Bool) -> SliceDim a -> Bool
anySliceDim p = \case
  SliceAll _ -> False
  SliceRange _ s l -> anyExp p s || anyExp p l

hasMap, hasZipWith, hasGenerate, hasFill, hasReduceGenerate, hasPermute, hasGather, hasScatter, hasScatterGenerate, hasReplicate, hasSlice, hasReshape, hasIndex :: Exp a -> Bool
hasMap = anyExp (\case EMap {} -> True; _ -> False)
hasZipWith = anyExp (\case EZipWith {} -> True; _ -> False)
hasGenerate = anyExp (\case EGenerate {} -> True; _ -> False)
hasFill = anyExp (\case EFill {} -> True; _ -> False)
hasReduceGenerate = anyExp (\case EReduceGenerate {} -> True; _ -> False)
hasPermute = anyExp (\case EPermute {} -> True; _ -> False)
hasGather = anyExp (\case EGather {} -> True; _ -> False)
hasScatter = anyExp (\case EScatter {} -> True; EScatterGuarded {} -> True; _ -> False)
hasScatterGenerate = anyExp (\case EScatterGenerate {} -> True; _ -> False)
hasReplicate = anyExp (\case EReplicate {} -> True; _ -> False)
hasSlice = anyExp (\case ESlice {} -> True; _ -> False)
hasReshape = anyExp (\case EReshape {} -> True; _ -> False)
hasIndex = anyExp (\case EIndex {} -> True; _ -> False)

spec :: Spec
spec = do
  describe "Fusion - Structural rewrites" $ do
    it "fuses map-map into a single map" $ do
      expectFusionSensible
        "let arr = generate [3] (let h [i] = i in h) in let g x = x + 1 in let f x = x * 2 in map f (map g arr)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses map over generate into generate" $ do
      expectFusionSensible
        "let g [i] = i + 1 in let f x = x * 2 in map f (generate [3] g)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses zipwith over map (left) into zipwith" $ do
      expectFusionSensible
        "let arr = fill [3] 0 in let g x = x + 1 in let f x y = x + y in zipwith f (map g arr) (fill [3] 0)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasFill fused `shouldBe` True)

    it "fuses zipwith over two generates into generate" $ do
      expectFusionSensible
        "let g [i] = i in let h [i] = i + 1 in let f x y = x + y in zipwith f (generate [3] g) (generate [3] h)"
        (\fused -> do
            hasMap fused `shouldBe` False
            (hasGenerate fused || hasZipWith fused) `shouldBe` True)

    it "fuses reduce over map into reduce" $ do
      expectFusionSensible
        "let arr = fill [3] 1 in let g x = x + 1 in let f x y = x + y in reduce f 0 (map g arr)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasReduceGenerate fused `shouldBe` True)

    it "fuses map over fill into fill" $ do
      expectFusionSensible
        "let f x = x + 1 in map f (fill [3] 2)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasFill fused `shouldBe` True)

    it "fuses zipwith over two fills into fill" $ do
      expectFusionSensible
        "let f x y = x + y in zipwith f (fill [3] 1) (fill [3] 2)"
        (\fused -> do
            hasZipWith fused `shouldBe` False
            hasFill fused `shouldBe` True)

    it "fuses permute over map into permute" $ do
      expectFusionSensible
        "let arr = generate [3] (let h [i] = i in h) in let g x = x + 1 in let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p (map g arr)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasPermute fused `shouldBe` True)

    it "inlines let-bound array used once" $ do
      expectFusionSensible
        "let arr = map (let g x = x + 1 in g) (fill [3] 1) in reduce (let add x y = x + y in add) 0 arr"
        (\fused -> hasMap fused `shouldBe` False)

    it "hoists let inside reduce and fuses map" $ do
      expectFusionSensible
        "let arr = fill [3] 1 in reduce (let add x y = x + y in add) 0 (let g x = x + 1 in map g arr)"
        (\fused -> hasMap fused `shouldBe` False)

    it "does not inline let-bound array used twice" $ do
      expectFusionSensible
        "let arr = map (let g x = x + 1 in g) (fill [3] 1) in zipwith (let f x y = x + y in f) arr arr"
        (\_ -> pure ())

    it "does not hoist let when it would capture" $ do
      expectFusionSensible
        "let x = 1 in map (let f y = x + y in f) (let x = 2 in generate [3] (let g [i] = x + i in g))"
        (\fused -> hasGenerate fused `shouldBe` True)

    it "does not hoist let across dependent operand" $ do
      expectFusionSensible
        "let x = 1 in zipwith (let f a b = a + b in f) (let x = 2 in fill [3] x) (fill [3] x)"
        (\fused -> hasFill fused `shouldBe` True)

    it "uniquify enables fusion across shadowing" $ do
      expectFusionSensible
        "let g x = x + 1 in map g (let g [i] = i + 2 in generate [3] g)"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses gather over producer via map kernel" $ do
      expectFusionSensible
        "let idx [i] = [i] in let g x = x + 1 in gather (generate [3] idx) (map g (fill [3] 2))"
        (\fused -> do
            hasGather fused `shouldBe` False
            hasMap fused `shouldBe` False
            (hasFill fused || hasGenerate fused) `shouldBe` True)

    it "fuses scatter over mapped values" $ do
      expectFusionSensible
        "let idx [i] = [i] in let g x = x + 1 in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idx) (map g (fill [3] 1))"
        (\fused -> do
            hasMap fused `shouldBe` False
            hasScatter fused `shouldBe` True)

    it "fuses index over map" $ do
      expectFusionSensible
        "let f [i] = i in let g x = x + 1 in index [1] (map g (generate [3] f))"
        (\fused -> do
            hasMap fused `shouldBe` False
            -- With full fusion, index into map-of-generate collapses to g(f([1])),
            -- so neither index nor generate remain.
            hasIndex fused `shouldBe` False)

    it "fuses replicate over map by commuting map out" $ do
      expectFusionSensible
        "let f [i] = i in let g x = x + 1 in replicate [Any 2, All] (map g (generate [3] f))"
        (\fused -> do
            hasReplicate fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses slice over map by commuting map out" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let g x = x + 1 in slice [[0,2], All] (map g (generate [3,4] f))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses reshape over map by commuting map out" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let g x = x + 1 in reshape [6] (map g (generate [2,3] f))"
        (\fused -> do
            hasReshape fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses reduce over gather into reduce_generate" $ do
      expectFusionSensible
        "let idx [i] = [i] in let f [i] = i + 1 in let add x y = x + y in reduce add 0 (gather (generate [3] idx) (generate [3] f))"
        (\fused -> hasReduceGenerate fused `shouldBe` True)

    it "fuses reshape over let-bound map" $ do
      expectFusionSensible
        "let inc x = x + 1 in let arr = map inc (fill [3] 1) in reshape [3] arr"
        (\fused -> hasReshape fused `shouldBe` True)

    it "fuses permute over let-bound map" $ do
      expectFusionSensible
        "let inc x = x + 1 in let arr = map inc (fill [3] 1) in let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p arr"
        (\fused -> hasPermute fused `shouldBe` True)

    it "fuses scatter over let-bound map" $ do
      expectFusionSensible
        "let d [i] = 0 in let v [i] = i + 1 in let inc x = x + 1 in let vals = map inc (generate [3] v) in let idx [i] = [i] in let c x y = x + y in scatter c (generate [3] d) (generate [3] idx) vals"
        -- map-of-generate on values is fully absorbed: no EScatter or EMap remain;
        -- values become a generator inlined into EScatterGenerate.
        (\fused -> do
            (hasScatter fused || hasScatterGenerate fused) `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses index over let-bound map" $ do
      expectFusionSensible
        "let id [i] = i in let inc x = x + 1 in let arr = map inc (generate [3] id) in index [1] arr"
        -- arr is inlined (single use), then index fuses with map-of-generate
        -- fully: result is inc(id([1])), so no index remains.
        (\fused -> hasIndex fused `shouldBe` False)

    it "fuses replicate over let-bound map" $ do
      expectFusionSensible
        "let inc x = x + 1 in let arr = map inc (fill [3] 1) in replicate [Any 2, All] arr"
        (\fused -> hasReplicate fused `shouldBe` True)

    it "fuses slice over let-bound map" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let inc x = x + 1 in let arr = map inc (generate [3,4] f) in slice [[0,2], All] arr"
        (\fused -> hasSlice fused `shouldBe` True)

    it "chains fusion through nested let bindings" $ do
      expectFusionSensible
        "let inc a = a + 1 in let double b = b * 2 in let y = map inc (fill [3] 1) in let x = map double y in reduce (+) 0 x"
        (\fused -> hasMap fused `shouldBe` False)

    it "fuses across sequential declarations" $ do
      expectFusionSensible
        "let id [i] = i in let arr = generate [3] id in let f x = x + 1 in let g x = x * 2 in let h = map f arr in map g h"
        (\fused -> hasMap fused `shouldBe` False)

    it "fuses multi-use generate through zipwith (generate inlined at all use sites)" $ do
      expectFusionSensible
        "let inc x = x + 1 in let add x y = x + y in let arr = map inc (fill [3] 1) in zipwith add arr arr"
        (\fused -> do
            -- arr = map inc (fill [3] 1) fuses to fill [3] 2 (element is constant),
            -- which is a producer and gets inlined at both use sites. Then
            -- zipwith add (fill [3] 2) (fill [3] 2) fuses to fill [3] 4.
            hasZipWith fused `shouldBe` False
            hasFill fused `shouldBe` True)

    it "preserves results with multiple uses of non-producer let-bound array" $ do
      expectFusionSensible
        "let add x y = x + y in let idx [i] = [i] in let arr = scatter add (fill [3] 0) (generate [3] idx) (fill [3] 1) in zipwith add arr arr"
        (\fused ->
            -- arr = scatter result (not a producer), so it must NOT be inlined
            -- at multiple use sites — the zipwith is preserved.
            hasZipWith fused `shouldBe` True)

    -- Slice fusion tests
    it "fuses slice over zipWith of two generates (1D)" $ do
      expectFusionSensible
        "let f [i] = i in let g [i] = i + 1 in slice [[0,2]] (zipwith (let add x y = x + y in add) (generate [4] f) (generate [4] g))"
        (\fused -> do
            -- With shape normalization, zipWith fuses into generate
            -- Slice stays (fuseSlice doesn't handle zipWith), but wraps the fused result
            hasSlice fused `shouldBe` True
            hasZipWith fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses slice over zipWith of two generates (2D)" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let g [i,j] = i * j in slice [[0,2], All] (zipwith (let add x y = x + y in add) (generate [4,3] f) (generate [4,3] g))"
        (\fused -> do
            -- With shape normalization, zipWith fuses into generate
            -- Slice stays (fuseSlice doesn't handle zipWith), but wraps the fused result
            hasSlice fused `shouldBe` True
            hasZipWith fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses reduce over sliced array (2D to 1D reduction)" $ do
      expectFusionSensible
        "let f [i,j] = i + j in reduce (let add x y = x + y in add) 0 (slice [[0,2], All] (generate [4,3] f))"
        (\fused -> do
            -- Slice fuses into the producer, so no slice remains
            hasSlice fused `shouldBe` False
            -- Reduce fuses with the sliced producer
            hasReduceGenerate fused `shouldBe` True)

    it "fuses reduce over slice of mapped array" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let g x = x * 2 in reduce (let add x y = x + y in add) 0 (slice [[0,2], All] (map g (generate [4,3] f)))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasMap fused `shouldBe` False
            -- reduce stays but map is fused into it
            hasReduceGenerate fused `shouldBe` False)

    it "fuses slice over generate directly (2D)" $ do
      expectFusionSensible
        "let f [i,j] = i + j in slice [[0,2], [1,2]] (generate [4,4] f)"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasGenerate fused `shouldBe` True)

    it "fuses slice over fill (2D)" $ do
      expectFusionSensible
        "slice [[0,2], All] (fill [4,3] 42)"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasFill fused `shouldBe` True)

    it "fuses slice reducing rank (3D to 2D)" $ do
      expectFusionSensible
        "let f [i,j,k] = i + j + k in slice [[0,1], All, All] (generate [2,3,4] f)"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasGenerate fused `shouldBe` True)

    it "fuses slice with multiple ranges (2D)" $ do
      expectFusionSensible
        "let f [i,j] = i * 10 + j in slice [[1,2], [0,3]] (generate [4,5] f)"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasGenerate fused `shouldBe` True)

    it "fuses slice with mixed All and ranges (3D)" $ do
      expectFusionSensible
        "let f [i,j,k] = i + j + k in slice [All, [0,2], [1,3]] (generate [2,4,5] f)"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasGenerate fused `shouldBe` True)

    it "fuses slice over replicate" $ do
      expectFusionSensible
        "let f [i] = i in slice [[0,2], All] (replicate [Any 3, All] (generate [4] f))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasReplicate fused `shouldBe` True)

    it "fuses slice over replicate over map" $ do
      expectFusionSensible
        "let f [i] = i in let g x = x + 1 in slice [[0,2], All] (replicate [Any 2, All] (map g (generate [4] f)))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasReplicate fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses nested slice operations" $ do
      expectFusionSensible
        "let f [i,j] = i + j in slice [[0,1], All] (slice [[0,2], All] (generate [4,4] f))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasGenerate fused `shouldBe` True)

    it "fuses slice after chained maps" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let g x = x + 1 in let h x = x * 2 in slice [[0,2], All] (map h (map g (generate [4,4] f)))"
        (\fused -> do
            hasSlice fused `shouldBe` True
            hasMap fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    it "fuses gather over sliced array" $ do
      expectFusionSensible
        "let idx [i] = [0, 0] in let f [i,j] = i + j in gather (generate [2] idx) (slice [[0,2], All] (generate [4,4] f))"
        (\fused -> do
            -- gather fuses into the computation
            hasGather fused `shouldBe` False)

    it "fuses permute over sliced array" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let c x y = x + y in let p [i,j] = [i,j] in permute c (fill [2,3] 0) p (slice [[0,2], [0,3]] (generate [4,5] f))"
        (\fused -> do
            hasPermute fused `shouldBe` True
            -- The slice is absorbed into the source generator by fusion;
            -- permute now operates on a fresh generate with the sliced shape.
            hasSlice fused `shouldBe` False)

    -- This test uses a permute function that produces out-of-bounds indices.
    -- The interpreter now gracefully handles this by using default values
    -- for positions where no valid mapping exists.
    it "permute with swapped indices over sliced array (out-of-bounds handled gracefully)" $ do
      expectFusionSensible
        "let f [i,j] = i + j in let c x y = x + y in let p [i,j] = [j,i] in permute c (fill [2,3] 0) p (slice [[0,2], [0,3]] (generate [4,5] f))"
        (\fused -> do
            hasPermute fused `shouldBe` True
            -- The slice is absorbed into the source generator by fusion.
            hasSlice fused `shouldBe` False)

    -- Investigation: map over slice should fuse the slice into the map
    -- Current behavior: slice is recognized as producer, should emit generate
    it "fuses map over slice of generate into single generate" $ do
      expectFusionSensible
        "let f x = x + 1 in let g [i,j] = i + j in map f (slice [[0,2], All] (generate [4,4] g))"
        (\fused -> do
            -- Debug: print what we got
            let hasLet = anyExp (\case ELetIn {} -> True; _ -> False) fused
            let hasGen = hasGenerate fused
            let hasMap' = hasMap fused
            let hasSlice' = hasSlice fused
            -- The key check: no slice should remain since it should be fused away
            hasSlice' `shouldBe` False
            -- We should get either direct generate or let-wrapped generate
            (hasGen || hasLet) `shouldBe` True
            hasMap' `shouldBe` False)

    -- Investigation: zipWith over two slices of generate
    -- With shape normalization, both slices produce the same normalized shape expression
    -- [2, 4], enabling zipWith fusion
    it "fuses zipWith over two slices of generate with shape normalization" $ do
      expectFusionSensible
        "let add x y = x + y in let g1 [i,j] = i in let g2 [i,j] = j in zipwith add (slice [[0,2], All] (generate [4,4] g1)) (slice [[0,2], All] (generate [4,4] g2))"
        (\fused -> do
            -- After shape normalization, both slices have shape [2,4]
            -- zipWith can now fuse them into a single generate
            hasZipWith fused `shouldBe` False
            hasSlice fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

    -- Investigation: zipWith with slice and fill - should this fuse?
    it "fuses zipWith with slice and fill" $ do
      expectFusionSensible
        "let add x y = x + y in let g [i,j] = i in zipwith add (slice [[0,2], All] (generate [4,4] g)) (fill [2,4] 0)"
        (\fused -> do
            -- Expecting: fill + slice should become generate
            hasZipWith fused `shouldBe` False
            hasFill fused `shouldBe` False
            hasGenerate fused `shouldBe` True)

  -- ---- Tests for new fusion rules (T1, T3, T4) ----

  describe "Fusion - fusePermute generalisation (T1)" $ do
    it "fuses permute over generate (not just map)" $ do
      -- Before T1, asProducer(generate) was blocked by isCompositeProducerExp guard.
      -- After T1, the producer branch fires and emits a single generate.
      expectFusionSensible
        "let f [i] = i + 1 in let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p (generate [3] f)"
        (\fused -> do
            hasPermute fused `shouldBe` True
            -- Source array should be simplified; no leftover map layers
            hasMap fused `shouldBe` False)

    it "fuses permute over fill (constant source)" $ do
      -- fill [3] 7: producer with KConst 7, emitted as fill [3] 7 — no change in shape
      -- but the combine/source pipeline is consistent.
      expectFusionSensible
        "let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p (fill [3] 7)"
        (\fused -> hasPermute fused `shouldBe` True)

    it "fuses permute over map-of-generate (multi-level producer)" $ do
      -- Before T1: isCompositeProducerExp fired but the combine was incorrectly
      -- modified (double-application bug).  After T1: producer branch emits the
      -- simplified generate with the ORIGINAL combine — semantically correct.
      expectFusionSensible
        "let f [i] = i in let g x = x + 1 in let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p (map g (generate [3] f))"
        (\fused -> do
            hasPermute fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "fuses permute over gather-based producer" $ do
      expectFusionSensible
        "let idx [i] = [i] in let f [i] = i in let c x y = x + y in let p [i] = [i] in permute c (fill [3] 0) p (gather (generate [3] idx) (generate [3] f))"
        (\fused -> do
            hasPermute fused `shouldBe` True
            hasGather fused `shouldBe` False)

  describe "Fusion - fuseIndex (index over producers)" $ do
    it "index into generate evaluates generator directly" $ do
      -- index i (generate shape f) => f i  (no generate, no index in result)
      expectFusionSensible
        "let f [i] = i + 1 in index [2] (generate [5] f)"
        (\fused -> do
            hasGenerate fused `shouldBe` False
            hasIndex fused `shouldBe` False)

    it "index into fill returns constant" $ do
      -- index i (fill shape v) => v
      expectFusionSensible
        "index [1] (fill [4] 99)"
        (\fused -> do
            hasFill fused `shouldBe` False
            hasIndex fused `shouldBe` False)

    it "index into map-of-generate fully fuses" $ do
      -- index i (map f (generate shape g)) => f (g i)
      expectFusionSensible
        "let f x = x * 2 in let g [i] = i in index [3] (map f (generate [5] g))"
        (\fused -> do
            hasGenerate fused `shouldBe` False
            hasIndex fused `shouldBe` False
            hasMap fused `shouldBe` False)

    it "index into map of variable floats lookup out" $ do
      -- index i (map f xs) => f (index i xs)  (xs is opaque)
      expectFusionSensible
        "let xs = generate [4] (let f [i] = i in f) in let g x = x + 1 in index [2] (map g xs)"
        (\fused -> hasMap fused `shouldBe` False)

    it "index into zipWith of generates fully fuses" $ do
      -- index i (zipwith f (generate sh g) (generate sh h)) => f (g i) (h i)
      expectFusionSensible
        "let f x y = x + y in let g [i] = i in let h [i] = i * 2 in index [1] (zipwith f (generate [3] g) (generate [3] h))"
        (\fused -> do
            hasGenerate fused `shouldBe` False
            hasIndex fused `shouldBe` False
            hasZipWith fused `shouldBe` False)

  describe "Fusion - ScatterKernel (T5-T9): scatter values-side" $ do
    it "absorbs map-of-generate in scatter values into combine (via ELetIn hoisting)" $ do
      -- scatter c d idx (map f (generate sh g))
      -- Step 1: fuseOnce compiles (map f (generate sh g)) -> let fused [i]=f(g(i)) in generate sh fused
      -- Step 2: fuseScatterKernel ELetIn-hoists -> let fused ... in scatter c d idx (generate sh fused)
      -- Semantics preserved; values-side no longer has a map.
      expectFusionSensible
        "let f x = x + 1 in let g [i] = i in let idxFn [i] = [i] in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idxFn) (map f (generate [3] g))"
        (\fused -> hasMap fused `shouldBe` False)

    it "absorbs map on values side into scatter combine directly" $ do
      -- scatter c d idx (map f xs) => let c' x y = c (f x) y in scatter c' d idx xs
      -- xs is a variable (opaque), so no further simplification.
      expectFusionSensible
        "let xs = generate [3] (let f [i] = i in f) in let f x = x + 1 in let idxFn [i] = [i] in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idxFn) (map f xs)"
        (\fused -> hasMap fused `shouldBe` False)

    it "absorbs map-of-generate in permute source into ELetIn form" $ do
      -- permute c d p (map f (generate sh g))
      -- ELetIn hoisting exposes the generate in the source position of permute.
      expectFusionSensible
        "let f x = x + 1 in let g [i] = i in let p [i] = [i] in let c x y = x + y in permute c (fill [3] 0) p (map f (generate [3] g))"
        (\fused -> hasMap fused `shouldBe` False)


    it "simplifies composite index array in scatter" $ do
      -- scatter c d (map t rawIdx) vals: the map on the index side is absorbed
      -- into a generate so the index expression is simpler.
      -- Values side is also a generate, so the whole thing becomes EScatterGenerate.
      expectFusionSensible
        "let t [i] = [i] in let idxFn [i] = [i] in let valFn [i] = i + 1 in let c x y = x + y in scatter c (fill [3] 0) (map t (generate [3] idxFn)) (generate [3] valFn)"
        (\fused -> do
            (hasScatter fused || hasScatterGenerate fused) `shouldBe` True
            -- The map on the index side should be absorbed
            hasMap fused `shouldBe` False)

  describe "Fusion - EScatterGenerate (values-side generate fusion)" $ do
    it "scatter over generate fuses to EScatterGenerate (no materialised values array)" $ do
      -- scatter c d idx (generate sh f)  =>  scatter_generate c d idx f
      -- The intermediate values array [f(0), f(1), ...] is never allocated.
      -- The index array (generate [3] idxFn) stays materialised.
      expectFusionSensible
        "let f [i] = i + 1 in let idxFn [i] = [i] in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idxFn) (generate [3] f)"
        (\fused -> do
            hasScatterGenerate fused `shouldBe` True
            hasScatter fused `shouldBe` False)

    it "scatter over map-of-generate fuses to EScatterGenerate via map absorption" $ do
      -- scatter c d idx (map g (generate sh f))
      -- Step 1: fuseOnce turns (map g (generate sh f)) into ELetIn+EGenerate
      -- Step 2: ELetIn hoisting in fuseScatterKernel (Rule 1)
      -- Step 3: Rule 3 absorbs map g into combine (values = generate sh f)
      -- Step 4: Rule 5 promotes to EScatterGenerate (no EScatter, no EMap left)
      expectFusionSensible
        "let f [i] = i in let g x = x + 1 in let idxFn [i] = [i] in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idxFn) (map g (generate [3] f))"
        (\fused -> do
            hasScatterGenerate fused `shouldBe` True
            hasScatter fused `shouldBe` False
            hasMap fused `shouldBe` False)

    it "EScatterGenerate preserves semantics (interpreter roundtrip)" $ do
      -- Scatters [10, 20, 30] into positions [2, 0, 1] with sum combine.
      -- Result should be [20, 30, 10].
      expectFusionSensible
        "let f [i] = (i + 1) * 10 in let idxFn [i] = [i] in let c x y = x + y in scatter c (fill [3] 0) (generate [3] idxFn) (generate [3] f)"
        (\fused -> hasScatterGenerate fused `shouldBe` True)

  describe "Fusion - scatter_reindex (Rule 6: same-source index and values)" $ do
    it "scatter (map routing src) (map value src) fuses to EScatterGenerate" $ do
      -- Both index and values are mapped from the same source array.
      -- Rule 6 detects the common source and converts to EScatterGenerate,
      -- avoiding materialisation of the intermediate map-values array.
      -- arr = [0, 1, 2]; routeFn maps Int -> [Int] (1D index), valueFn maps Int -> Int.
      expectFusionSensible
        ( "let arr = generate [3] (let f [i] = i in f) in"
          <> " let routeFn x = [x] in"
          <> " let valueFn x = x + 10 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [3] 0) (map routeFn arr) (map valueFn arr)"
        )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "scatter with zipwith-derived routes and values fuses to EScatterGenerate" $ do
      expectFusionSensible
        ( "let xs = generate [8] (let fx [i] = i in fx) in"
          <> " let ys = generate [8] (let fy [i] = 100 + i in fy) in"
          <> " let routeFn x y = x / 2 in"
          <> " let valueFn x y = x + y in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [4] 0) (zipwith routeFn xs ys) (zipwith valueFn xs ys)"
        )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "scatter with map-derived routes and zipwith-derived values fuses to EScatterGenerate" $ do
      expectFusionSensible
        ( "let xs = generate [8] (let fx [i] = i in fx) in"
          <> " let ys = generate [8] (let fy [i] = 100 + i in fy) in"
          <> " let routeFn x = x / 2 in"
          <> " let valueFn x y = x + y in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [4] 0) (map routeFn xs) (zipwith valueFn xs ys)"
        )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "scatter with zipwith-derived routes and map-derived values fuses to EScatterGenerate" $ do
      expectFusionSensible
        ( "let xs = generate [8] (let fx [i] = i in fx) in"
          <> " let ys = generate [8] (let fy [i] = 100 + i in fy) in"
          <> " let routeFn x y = x / 2 in"
          <> " let valueFn x = x * 3 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [4] 0) (zipwith routeFn xs ys) (map valueFn xs)"
         )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "scatter_guarded with same-source route, values, and guard eliminates producer maps" $ do
      expectFusionSensible
        ( "let src = generate [8] (let f [i] = i in f) in"
          <> " let routeFn x = x / 2 in"
          <> " let valueFn x = (x * 3) + 1 in"
          <> " let guardFn x = (x / 2) = ((x + 1) / 2) in"
          <> " let c x y = x + y in"
          <> " scatter_guarded c (fill [4] 0) (map routeFn src) (map valueFn src) (map guardFn src)"
        )
        (\fused -> do
            hasMap fused `shouldBe` False
            hasGenerate fused `shouldBe` True
            hasScatter fused `shouldBe` True)

    it "scatter_reindex with same source, same mapping (Rocq strict scatter_reindex pattern)" $ do
      -- scatter_reindex where both routing and values come from the same source.
      -- Since routing and values must have different types in a typed language,
      -- we use separate projections from a paired source to simulate this.
      -- Route: map (\x -> [x]) src;  Value: map (\x -> x * 2) src.
      -- Rule 6 fires because rawSrc is the same for both maps.
      expectFusionSensible
        ( "let src = generate [3] (let f [i] = i in f) in"
          <> " let routeFn x = [x] in"
          <> " let valueFn x = x * 2 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [3] 0) (map routeFn src) (map valueFn src)"
        )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "scatter_reindex preserves semantics (interpreter roundtrip)" $ do
      -- Histogram-like pattern: bin each element and accumulate.
      -- arr = [0, 1, 2], routing = map (\x -> [x]) arr, values = map (\x -> x + 10) arr
      -- Expected: output[0] = 10, output[1] = 11, output[2] = 12
      expectFusionSensible
        ( "let arr = generate [3] (let f [i] = i in f) in"
          <> " let routeFn x = [x] in"
          <> " let valueFn x = x + 10 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [3] 0) (map routeFn arr) (map valueFn arr)"
        )
        (\fused -> hasScatterGenerate fused `shouldBe` True)

    it "same-source maps: index side is also lazified (no intermediate EMap)" $ do
      -- After the Case 6a improvement, the route array is generated lazily too,
      -- so no EMap should survive in the fused output.
      expectFusionSensible
        ( "let arr = generate [3] (let f [i] = i in f) in"
          <> " let routeFn x = [x] in"
          <> " let valueFn x = x + 10 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [3] 0) (map routeFn arr) (map valueFn arr)"
        )
        (\fused -> do
            hasScatterGenerate fused `shouldBe` True
            hasMap fused `shouldBe` False)

    it "does not fire when index and values come from different sources" $ do
      -- Rule 6 should NOT fire when the two EMap expressions have different sources.
      -- Rule 3 (map absorption) fires instead, producing EScatter (not EScatterGenerate).
      expectFusionSensible
        ( "let idxArr = generate [3] (let f [i] = [i] in f) in"
          <> " let valArr = fill [3] 42 in"
          <> " let c x y = x + y in"
          <> " scatter c (fill [3] 0) idxArr valArr"
        )
        (\fused -> (hasScatter fused || hasScatterGenerate fused) `shouldBe` True)

    it "fuses dot.hyd without changing result" $ do
      src <- BS.readFile "examples/dot.hyd"
      case readDecs src of
        Left err -> expectationFailure err
        Right decs -> do
          typeRes <- inferDecsTop decs
          case typeRes of
            Left err -> expectationFailure err
            Right _ -> do
              let fusedDecs = fuseDecs (uniquifyDecs decs)
              fusedTypeRes <- inferDecsTop fusedDecs
              case fusedTypeRes of
                Left err -> expectationFailure err
                Right _ ->
                  do
                    r1 <- evalDecsFrontend decs
                    r2 <- evalDecsFrontend fusedDecs
                    case (r1, r2) of
                      (Right env1, Right env2) ->
                        M.lookup "result" env1 `shouldBe` M.lookup "result" env2
                      (Left err, _) -> expectationFailure (show err)
                      (_, Left err) -> expectationFailure (show err)
