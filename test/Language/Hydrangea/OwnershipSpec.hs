{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.OwnershipSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map.Strict qualified as M
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom (..), CElemType (..), CType (..), RHS (..))
import Language.Hydrangea.Ownership (OwnershipResult (..), insertOwnership)
import Control.Exception (evaluate)
import Test.Hspec

arr :: CType
arr = CTArray CTInt64

pairTy :: CType
pairTy = CTPair arr arr

run :: [(ByteString, CType)] -> [ByteString] -> [Stmt] -> [Stmt]
run types params body =
  procBody (orProc (insertOwnership (const False) (M.fromList types) (mkProc "p" params body)))

alloc :: ByteString -> Stmt
alloc v = SAssign v (RArrayAlloc (AVar "shp"))

free :: ByteString -> Stmt
free v = SAssign "__hyd_discard" (RArrayFree (AVar v))

retainOf :: Stmt -> Maybe (ByteString, Atom)
retainOf stmt = case stmt of
  SAssign v (RCall "hyd_array_retain" [a]) -> Just (v, a)
  _ -> Nothing

frees :: [Stmt] -> [ByteString]
frees stmts = [v | SAssign "__hyd_discard" (RArrayFree (AVar v)) <- stmts]

loop :: [Stmt] -> Stmt
loop = SLoop (LoopSpec ["i"] [IConst 4] Serial Nothing LoopIterate [])

spec :: Spec
spec = describe "Ownership" $ do
  it "releases a local right after its last use, and not the returned one" $ do
    let body =
          [ alloc "a"
          , SAssign "b" (RCall "f" [AVar "a"])
          , SAssign "n" (RShapeSize (AVar "shp"))
          , SReturn (AVar "b")
          ]
    run [("a", arr), ("b", arr)] [] body
      `shouldBe` [alloc "a", SAssign "b" (RCall "f" [AVar "a"]), free "a", SAssign "n" (RShapeSize (AVar "shp")), SReturn (AVar "b")]

  it "releases a value nobody reads right after it is made" $ do
    run [("a", arr)] [] [alloc "a", SReturn (AInt 0)]
      `shouldBe` [alloc "a", free "a", SReturn (AInt 0)]

  it "moves a copy whose source dies, and retains one whose source lives on" $ do
    let moved = run [("a", arr), ("b", arr)] [] [alloc "a", SAssign "b" (RAtom (AVar "a")), SReturn (AVar "b")]
    moved `shouldBe` [alloc "a", SAssign "b" (RAtom (AVar "a")), SReturn (AVar "b")]
    let kept =
          run
            [("a", arr), ("b", arr)]
            []
            [alloc "a", SAssign "b" (RAtom (AVar "a")), SAssign "t" (RCall "g" [AVar "a"]), SReturn (AVar "b")]
    map retainOf kept `shouldContain` [Just ("b", AVar "a")]
    frees kept `shouldBe` ["a"]

  it "retains a returned parameter, which the caller still owns" $ do
    let out = run [("x", arr)] ["x"] [SReturn (AVar "x")]
    case out of
      [SAssign r (RCall "hyd_array_retain" [AVar "x"]), SReturn (AVar r')] -> r `shouldBe` r'
      _ -> expectationFailure (show out)
    frees out `shouldBe` []

  it "moves arrays into a pair and releases the rest of a pair it projects out of" $ do
    let body =
          [ alloc "a"
          , alloc "b"
          , SAssign "p" (RPairMake (CEArray CTInt64) (CEArray CTInt64) (AVar "a") (AVar "b"))
          , SAssign "x" (RPairFst (CEArray CTInt64) (AVar "p"))
          , SReturn (AVar "x")
          ]
        out = run [("a", arr), ("b", arr), ("p", pairTy), ("x", arr)] [] body
    take 4 out `shouldBe` take 4 body
    -- p.snd is projected into a temporary and freed; p.fst moved into x.
    length (frees out) `shouldBe` 1
    [v | SAssign v (RPairSnd _ (AVar "p")) <- out] `shouldSatisfy` ((== 1) . length)
    last out `shouldBe` SReturn (AVar "x")

  it "retains both uses when one variable fills both halves of a pair" $ do
    let body =
          [ alloc "a"
          , SAssign "p" (RPairMake (CEArray CTInt64) (CEArray CTInt64) (AVar "a") (AVar "a"))
          , SReturn (AVar "p")
          ]
        out = run [("a", arr), ("p", pairTy)] [] body
    length [() | Just _ <- map retainOf out] `shouldBe` 1
    frees out `shouldBe` []

  it "releases on each branch of an if what that branch doesn't read" $ do
    let body =
          [ alloc "a"
          , SIf (AVar "c") [SAssign "t" (RCall "g" [AVar "a"])] []
          , SReturn (AInt 0)
          ]
        out = run [("a", arr)] [] body
    case out of
      [_, SIf _ thn els, _] -> do
        frees thn `shouldBe` ["a"]
        frees els `shouldBe` ["a"]
      _ -> expectationFailure (show out)

  it "releases an iterate's old state inside the loop and its final state after it" $ do
    let body =
          [ alloc "init"
          , SAssign "cur" (RAtom (AVar "init"))
          , loop [SAssign "next" (RCall "step" [AVar "cur"]), SAssign "cur" (RAtom (AVar "next"))]
          , SAssign "s" (RArrayShape (AVar "cur"))
          , SReturn (AInt 0)
          ]
        out = run [("init", arr), ("cur", arr), ("next", arr)] [] body
    out
      `shouldBe` [ alloc "init"
                 , SAssign "cur" (RAtom (AVar "init"))
                 , loop [SAssign "next" (RCall "step" [AVar "cur"]), free "cur", SAssign "cur" (RAtom (AVar "next"))]
                 , SAssign "s" (RArrayShape (AVar "cur"))
                 , free "cur"
                 , SReturn (AInt 0)
                 ]

  it "releases a per-trip temporary at its last use in the trip" $ do
    let body =
          [ loop [alloc "tmp", SAssign "k" (RCall "g" [AVar "tmp"]), SAssign "m" (RCall "h" [AVar "k"])]
          , SReturn (AInt 0)
          ]
        out = run [("tmp", arr)] [] body
    out
      `shouldBe` [ loop [alloc "tmp", SAssign "k" (RCall "g" [AVar "tmp"]), free "tmp", SAssign "m" (RCall "h" [AVar "k"])]
                 , SReturn (AInt 0)
                 ]

  it "swaps ping-pong buffers without counting, and releases the spare after the loop" $ do
    let body =
          [ alloc "cur"
          , alloc "spare"
          , loop
              [ SArrayWrite (AVar "spare") (AInt 0) (AInt 1)
              , SAssign "tmp" (RAtom (AVar "cur"))
              , SAssign "cur" (RAtom (AVar "spare"))
              , SAssign "spare" (RAtom (AVar "tmp"))
              ]
          , SReturn (AVar "cur")
          ]
        out = run [("cur", arr), ("spare", arr), ("tmp", arr)] [] body
    out `shouldBe` take 3 body ++ [free "spare", SReturn (AVar "cur")]

  it "keeps the loop state alive on a break, and releases it once after the loop" $ do
    let body =
          [ alloc "cur"
          , loop
              [ SIf (AVar "c") [SBreak] []
              , SAssign "next" (RCall "step" [AVar "cur"])
              , SAssign "cur" (RAtom (AVar "next"))
              ]
          , SAssign "s" (RArrayShape (AVar "cur"))
          , SReturn (AInt 0)
          ]
        out = run [("cur", arr), ("next", arr)] [] body
    case out of
      [_, SLoop _ (SIf _ thn els : rest), _, afterFree, _] -> do
        frees thn `shouldBe` []
        frees els `shouldBe` []
        frees rest `shouldBe` ["cur"]
        afterFree `shouldBe` free "cur"
      _ -> expectationFailure (show out)

  it "releases, at the start of each trip, loop state the trip overwrites before reading" $ do
    let body =
          [ alloc "x"
          , loop [SAssign "x" (RCall "mk" []), SAssign "k" (RCall "g" [AVar "x"])]
          , SReturn (AVar "x")
          ]
        out = run [("x", arr)] [] body
    out
      `shouldBe` [ alloc "x"
                 , loop [free "x", SAssign "x" (RCall "mk" []), SAssign "k" (RCall "g" [AVar "x"])]
                 , SReturn (AVar "x")
                 ]

  it "borrows a copy that never outlives its source" $ do
    let body =
          [ alloc "a"
          , SAssign "b" (RAtom (AVar "a"))
          , SAssign "k" (RCall "g" [AVar "b"])
          , SAssign "m" (RCall "g" [AVar "a"])
          , SReturn (AInt 0)
          ]
        out = run [("a", arr), ("b", arr)] [] body
    out `shouldBe` take 4 body ++ [free "a", SReturn (AInt 0)]

  it "moves each component out of a pair taken apart by consecutive projections" $ do
    let e = CEArray CTInt64
        body =
          [ alloc "a"
          , alloc "b"
          , SAssign "p" (RPairMake e e (AVar "a") (AVar "b"))
          , SAssign "x" (RPairFst e (AVar "p"))
          , SAssign "y" (RPairSnd e (AVar "p"))
          , SAssign "k" (RCall "g" [AVar "x", AVar "y"])
          , SReturn (AInt 0)
          ]
        out = run [("a", arr), ("b", arr), ("p", pairTy), ("x", arr), ("y", arr)] [] body
    [() | Just _ <- map retainOf out] `shouldBe` []
    out `shouldBe` take 6 body ++ [free "x", free "y", SReturn (AInt 0)]

  it "retains a borrowed argument before handing it to hyd_array_own" $ do
    let out = run [("x", arr), ("y", arr)] ["x"] [SAssign "y" (RCall "hyd_array_own" [AVar "x"]), SReturn (AVar "y")]
    case out of
      [SAssign r (RCall "hyd_array_retain" [AVar "x"]), SAssign "y" (RCall "hyd_array_own" [AVar r']), SReturn (AVar "y")] ->
        r `shouldBe` r'
      _ -> expectationFailure (show out)

  it "rejects a procedure that already frees an array" $ do
    let body = [alloc "a", free "a", SReturn (AInt 0)]
    evaluate (length (run [("a", arr)] [] body)) `shouldThrow` anyErrorCall
