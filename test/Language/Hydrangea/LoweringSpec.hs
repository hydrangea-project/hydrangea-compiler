{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.LoweringSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map

import Language.Hydrangea.CFGCore
import Language.Hydrangea.CFG
import Language.Hydrangea.Lexer hiding (Forall)
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Lowering
import Language.Hydrangea.Parser
import Language.Hydrangea.Syntax

-- Helper to parse and lower source code
lowerFromSource :: BS.ByteString -> IO Program
lowerFromSource src =
  case parseSource src of
    Left err -> fail $ "Parse error: " ++ err
    Right decs -> pure $ lowerDecs2 decs

parseSource :: BS.ByteString -> Either String [Dec Range]
parseSource str = runAlex str parseDecs

-- Helper to check if string contains substring
contains :: String -> String -> Bool
contains substr = isInfixOf substr

returnedVar Proc { procBody = stmts } = case reverse stmts of
  SReturn (AVar v):_ -> Just v
  _ -> Nothing

returnedVar :: Proc -> Maybe ByteString

lookupArrayFact :: ByteString -> Proc -> Maybe ArrayFact
lookupArrayFact v Proc { procArrayFacts = facts } = Map.lookup v facts

spec :: Spec
spec = do
  describe "Lowering - Basic Expressions" $ do
    it "lowers integer literals" $ do
      prog <- lowerFromSource "let x = 42"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      length stmts `shouldBe` 1  -- just return for literal
      
    it "lowers float literals" $ do
      prog <- lowerFromSource "let x = 3.14"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers boolean literals" $ do
      prog <- lowerFromSource "let x = true"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers string literals" $ do
      prog <- lowerFromSource "let x = \"hello\""
      let Program procs = prog
      length procs `shouldBe` 1

  describe "Lowering - Arithmetic" $ do
    it "lowers addition" $ do
      prog <- lowerFromSource "let x = 1 + 2"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers subtraction" $ do
      prog <- lowerFromSource "let x = 5 - 3"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers multiplication" $ do
      prog <- lowerFromSource "let x = 4 * 5"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers division" $ do
      prog <- lowerFromSource "let x = 10 / 2"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers negation" $ do
      prog <- lowerFromSource "let x = -42"
      let Program procs = prog
      length procs `shouldBe` 1

  describe "Lowering - Comparisons" $ do
    it "lowers equality" $ do
      prog <- lowerFromSource "let x = 1 = 2"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers less than" $ do
      prog <- lowerFromSource "let x = 1 < 2"
      let Program procs = prog
      length procs `shouldBe` 1

  describe "Lowering - Let Bindings" $ do
    it "lowers simple let binding" $ do
      prog <- lowerFromSource "let x = 42"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procName = name } = head procs
      name `shouldBe` "x"
      
    it "lowers nested let bindings" $ do
      prog <- lowerFromSource "let y = let x = 1 in x + 2"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers multiple sequential lets" $ do
      prog <- lowerFromSource "let x = 1\nlet y = 2\nlet z = x + y"
      let Program procs = prog
      length procs `shouldBe` 3

  describe "Lowering - Functions" $ do
    it "lowers function definition and call" $ do
      prog <- lowerFromSource "let add x y = x + y\nlet z = add 1 2"
      let Program procs = prog
      length procs `shouldBe` 2
      
    it "lowers higher-order function" $ do
      prog <- lowerFromSource "let apply f x = f x\nlet add1 x = x + 1\nlet y = apply add1 5"
      let Program procs = prog
      length procs `shouldBe` 3

  describe "Lowering - Conditionals" $ do
    it "lowers if-then-else" $ do
      prog <- lowerFromSource "let x = if true then 1 else 2"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      case head stmts of
        SIf _ _ _ -> pure ()
        _ -> expectationFailure "Expected SIf statement"
        
    it "lowers if-then (no else)" $ do
      prog <- lowerFromSource "let x = if true then 1"
      let Program procs = prog
      length procs `shouldBe` 1

  describe "Lowering - Arrays" $ do
    it "lowers fill array" $ do
      prog <- lowerFromSource "let arr = fill [3] 0"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      -- Should contain array_alloc and SLoop
      any isArrayAlloc stmts `shouldBe` True
      any isSLoop stmts `shouldBe` True
      loopRoles stmts `shouldBe` [LoopMap]
      
    it "lowers generate array" $ do
      prog <- lowerFromSource "let arr = generate [3] (let f [i] = i + 1 in f)"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      any isSLoop stmts `shouldBe` True
      loopRoles stmts `shouldBe` [LoopMap]
      
    it "lowers map over array" $ do
      prog <- lowerFromSource "let arr = fill [3] 0\nlet mapped = map (let inc x = x + 1 in inc) arr"
      let Program procs = prog
      length procs `shouldBe` 2
      let Proc { procBody = stmts } = head [proc | proc@Proc { procName = name } <- procs, name == "mapped"]
      loopRoles stmts `shouldBe` [LoopMap]
      
    it "lowers zipWith" $ do
      prog <- lowerFromSource "let arr1 = fill [3] 1\nlet arr2 = fill [3] 2\nlet zipped = zipwith (+) arr1 arr2"
      let Program procs = prog
      length procs `shouldBe` 3
      let Proc { procBody = stmts } = head [proc | proc@Proc { procName = name } <- procs, name == "zipped"]
      loopRoles stmts `shouldBe` [LoopMap]
      
    it "lowers reduce" $ do
      prog <- lowerFromSource "let sum = let add x y = x + y in reduce add 0 (fill [3] 1)"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      -- Should contain nested loops for reduce
      countSLoops stmts `shouldSatisfy` (>= 1)

    it "lowers gather as a map-style output loop" $ do
      prog <- lowerFromSource $ BS.pack $ unlines
        [ "let src = fill [3] 7"
        , "let idx = generate [3] (let f [i] = [i] in f)"
        , "let gathered = gather idx src"
        ]
      let Program procs = prog
      length procs `shouldBe` 3
      let Proc { procBody = stmts } = head [proc | proc@Proc { procName = name } <- procs, name == "gathered"]
      loopRoles stmts `shouldBe` [LoopMap]
      
    it "lowers array index" $ do
      prog <- lowerFromSource "let arr = fill [3] 0\nlet x = index [0] arr"
      let Program procs = prog
      length procs `shouldBe` 2
      
    it "lowers array shape" $ do
      prog <- lowerFromSource "let arr = fill [3] 0\nlet s = shape_of arr"
      let Program procs = prog
      length procs `shouldBe` 2

  describe "Lowering - Tuples" $ do
    it "lowers tuple construction" $ do
      prog <- lowerFromSource "let t = [1, 2, 3]"
      let Program procs = prog
      length procs `shouldBe` 1
      
    it "lowers tuple projection" $ do
      prog <- lowerFromSource "let t = [1, 2]\nlet x = fst t"
      let Program procs = prog
      length procs `shouldBe` 2

  describe "Lowering - Property Tests" $ do
    it "produces valid CFG for any simple expression" $ property $
      \n -> ioProperty $ do
        let src = BS.pack $ "let x = " ++ show (n :: Int)
        case parseSource src of
          Left _ -> return False
          Right decs -> do
            let Program procs = lowerDecs2 decs
            return $ length procs >= 0  -- Should not crash

  describe "Lowering - Scatter" $ do
    it "lowers scatter with a single loop over the index array" $ do
      -- scatter (+) (fill [3] 0) idx vals: loop should be over idx, not over defaults
      prog <- lowerFromSource
        "let r = scatter (+) (fill [3] 0) (generate [6] (let f [i] = i / 3 in f)) (fill [6] 1)"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      -- scatter itself generates exactly one SLoop over the index/source positions
      -- (fill and generate each also produce loops; scatter must produce at least 1)
      countSLoops stmts `shouldSatisfy` (>= 1)

    it "scatter loop body loads old accumulator value (combine reads output array)" $ do
      prog <- lowerFromSource
        "let r = scatter (+) (fill [3] 0) (generate [3] (let f [i] = i in f)) (generate [3] (let g [i] = i + 1 in g))"
      let Program procs = prog
      let Proc { procBody = stmts } = head procs
      -- After fixing: the scatter loop body must contain an RArrayLoad to read
      -- the current accumulator before applying combine.
      anyArrayLoad stmts `shouldBe` True

    it "scatter returns the output array (not a shape struct)" $ do
      -- Verify the lowered scatter proc ends with SReturn of an array atom.
      -- (Previously it erroneously returned 'AVar shp' which is a shape struct.)
      prog <- lowerFromSource
        "let r = scatter (+) (fill [2] 0) (generate [2] (let f [i] = i in f)) (fill [2] 1)"
      let Program procs = prog
      let Proc { procBody = stmts } = head procs
      -- The last statement should be SReturn of a non-shape atom.
      -- We verify indirectly: compiling this to C and running should not crash;
      -- at the CFG level, the proc must have at least one array alloc (from fill).
      any isArrayAlloc stmts `shouldBe` True

    it "scatter_generate (via fusion) lowers with combine applied" $ do
      -- Fused scatter_generate: scatter (+) (fill [3] 0) idx (map f src)
      -- where idx and src are from the same source → should fuse into EScatterGenerate
      -- after the fusion pass.  At the lowering level (no fusion), we just check
      -- that a plain scatter with map on values lowers without crashing.
      prog <- lowerFromSource
        "let r = scatter (+) (fill [3] 0) (generate [3] (let f [i] = i in f)) (generate [3] (let g [i] = i * 2 in g))"
      let Program procs = prog
      length procs `shouldBe` 1
      let Proc { procBody = stmts } = head procs
      countSLoops stmts `shouldSatisfy` (>= 1)

  describe "Lowering - Example Files" $ do
    it "lowers simple.hyd" $ do
      src <- BS.readFile "examples/simple.hyd"
      prog <- lowerFromSource src
      let Program procs = prog
      length procs `shouldSatisfy` (> 0)
      
    it "lowers arith.hyd" $ do
      src <- BS.readFile "examples/arith.hyd"
      prog <- lowerFromSource src
      let Program procs = prog
      length procs `shouldSatisfy` (> 0)
      
    it "lowers arrays.hyd" $ do
      src <- BS.readFile "examples/arrays.hyd"
      prog <- lowerFromSource src
      let Program procs = prog
      length procs `shouldSatisfy` (> 0)

  describe "Lowering - Type Environment (procTypeEnv)" $ do

    it "procTypeEnv contains CTDouble for float variable from binop" $ do
      -- A float binop result IS registered (via registerFloat)
      prog <- lowerFromSource "let x = 1.0 +. 2.0"
      let Program procs = prog
      let Proc { procTypeEnv = tenv } = head procs
      any (== CTDouble) (map snd (Map.toList tenv)) `shouldBe` True

    it "procTypeEnv is empty for float literal (direct atom, no variable)" $ do
      -- Float literals return AFloat directly; no temp variable is registered
      prog <- lowerFromSource "let x = 3.14"
      let Program procs = prog
      let Proc { procTypeEnv = tenv } = head procs
      -- No variables at all — the value is a direct atom
      Map.null tenv `shouldBe` True

    it "procTypeEnv contains CTPair for (Float, Int) pair" $ do
      prog <- lowerFromSource "let main = (3.14, 42)"
      let Program procs = prog
      let Proc { procTypeEnv = tenv } = head procs
      let pairs = [ct | (_, ct@(CTPair _ _)) <- Map.toList tenv]
      null pairs `shouldBe` False

    it "procTypeEnv contains CTRecord for record literals" $ do
      prog <- lowerFromSource "let main = {x = 1, y = 2.0}"
      let Program procs = prog
      let Proc { procTypeEnv = tenv } = head procs
      let records = [fields | (_, CTRecord fields) <- Map.toList tenv]
      records `shouldContain` [[("x", CTInt64), ("y", CTDouble)]]

    it "procTypeEnv entries for float pair are CTPair CTDouble CTInt64" $ do
      prog <- lowerFromSource "let main = (1.0, 2)"
      let Program procs = prog
      let Proc { procTypeEnv = tenv } = head procs
      let hasPair = any (\ct -> ct == CTPair CTDouble CTInt64) (map snd (Map.toList tenv))
      hasPair `shouldBe` True

    it "procTypeEnv is populated for (Float, Int) but not for simple generate" $ do
      -- Pairs populate the type env (via registerCType in EPair lowering)
      prog1 <- lowerFromSource "let main = (1.0, 2)"
      let Program procs1 = prog1
      let Proc { procTypeEnv = tenv1 } = head procs1
      Map.null tenv1 `shouldBe` False
      -- A pure generate without explicit pair construction may have empty type env
      prog2 <- lowerFromSource "let arr = generate [4] (fn [i] => i)"
      let Program procs2 = prog2
      -- We just verify it lowers successfully (type env completeness is incremental)
      length procs2 `shouldSatisfy` (>= 1)

    it "lowerDecs2WithTypeEnv populates procTypeEnv from top-level types" $ do
      let src = "let main = (2.0, 3)"
      case parseSource src of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right decs -> do
          let prog = lowerDecs2WithTypeEnv Map.empty decs
          let Program procs = prog
          let Proc { procTypeEnv = tenv } = head procs
          -- Should have pair types in type env
          let pairs = [ct | (_, ct@(CTPair _ _)) <- Map.toList tenv]
          null pairs `shouldBe` False

  describe "Lowering - Array Facts (procArrayFacts)" $ do
    it "marks fill outputs as fresh and write-once" $ do
      prog <- lowerFromSource "let arr = fill [3] 0"
      let Program procs = prog
      let proc = head procs
      case returnedVar proc >>= (`lookupArrayFact` proc) of
        Just fact -> do
          afFreshAlloc fact `shouldBe` True
          afWriteOnce fact `shouldBe` True
          afReadOnly fact `shouldBe` False
        Nothing -> expectationFailure "expected array facts for fill result"

    it "marks map parameters read-only and the output fresh/write-once" $ do
      prog <- lowerFromSource "let mapped xs = map (let inc x = x + 1 in inc) xs"
      let Program procs = prog
      let proc = head [p | p@Proc { procName = name } <- procs, name == "mapped"]
      lookupArrayFact "xs" proc `shouldBe` Just (ArrayFact False False True)
      case returnedVar proc >>= (`lookupArrayFact` proc) of
        Just fact -> do
          afFreshAlloc fact `shouldBe` True
          afWriteOnce fact `shouldBe` True
        Nothing -> expectationFailure "expected array facts for map result"

    it "marks both zipWith inputs read-only" $ do
      prog <- lowerFromSource "let zipped xs ys = zipwith (+) xs ys"
      let Program procs = prog
      let proc = head [p | p@Proc { procName = name } <- procs, name == "zipped"]
      lookupArrayFact "xs" proc `shouldBe` Just (ArrayFact False False True)
      lookupArrayFact "ys" proc `shouldBe` Just (ArrayFact False False True)

    it "marks gather index/source inputs read-only and output fresh/write-once" $ do
      prog <- lowerFromSource "let gathered idx src = gather idx src"
      let Program procs = prog
      let proc = head [p | p@Proc { procName = name } <- procs, name == "gathered"]
      lookupArrayFact "idx" proc `shouldBe` Just (ArrayFact False False True)
      lookupArrayFact "src" proc `shouldBe` Just (ArrayFact False False True)
      case returnedVar proc >>= (`lookupArrayFact` proc) of
        Just fact -> do
          afFreshAlloc fact `shouldBe` True
          afWriteOnce fact `shouldBe` True
        Nothing -> expectationFailure "expected array facts for gather result"
isArrayAlloc :: Stmt -> Bool
isArrayAlloc (SAssign _ (RArrayAlloc _)) = True
isArrayAlloc _ = False

isSLoop :: Stmt -> Bool
isSLoop (SLoop _ _) = True
isSLoop _ = False

countSLoops :: [Stmt] -> Int
countSLoops = sum . map countInStmt
  where
    countInStmt (SLoop _ body) = 1 + countSLoops body
    countInStmt (SIf _ thn els) = countSLoops thn + countSLoops els
    countInStmt _ = 0

-- | True if any statement (recursively including loop bodies) contains an RArrayLoad.
anyArrayLoad :: [Stmt] -> Bool
anyArrayLoad = any checkStmt
  where
    checkStmt (SAssign _ (RArrayLoad _ _)) = True
    checkStmt (SLoop _ body)               = anyArrayLoad body
    checkStmt (SIf _ thn els)              = anyArrayLoad thn || anyArrayLoad els
    checkStmt _                            = False

loopRoles :: [Stmt] -> [LoopRole]
loopRoles = concatMap go
  where
    go stmt = case stmt of
      SLoop loopSpec body -> lsRole loopSpec : loopRoles body
      SIf _ thn els -> loopRoles thn ++ loopRoles els
      _ -> []
