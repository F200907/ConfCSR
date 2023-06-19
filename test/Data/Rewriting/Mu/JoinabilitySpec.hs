{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Data.Rewriting.Mu.JoinabilitySpec where

import Data.Rewriting.Mu.CriticalPair (extendedCPs, lhCPs, muCPs)
import Data.Rewriting.Mu.Joinability (Joinability (Counterexample, Joinable, Undecidable), areTermsJoinable, isECPJoinable, isLHCPInstanceJoinable)
import Data.Rewriting.Term (Term (Fun, Var), rename)
import SpecUtil (cstrsFromString, replacementMapFromList, termFromString)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "Joinability" $ do
  describe "areTermsJoinable" $ do
    it "input in normal form" $ do
      let mu = replacementMapFromList [("f", [1]), ("g", [0]), ("h", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(a, a)", "g(x)"), ("g(x)", "a"), ("h(x)", "b")]
      let t1 = termFromString ["x"] "f(f(x, a), b)"
      let t2 = termFromString ["x"] "f(g(x), a)"
      areTermsJoinable mu cstrs (-1) t1 t2 `shouldBe'` Just False
    it "common reduct" $ do
      let mu = replacementMapFromList [("f", [0]), ("g", []), ("a", [])]
      let cstrs = cstrsFromString ["x"] [("f(g(x))", "g(x)"), ("g(x)", "a")]
      let t1 = termFromString ["x"] "f(f(f(g(x))))"
      let t2 = termFromString ["x"] "f(g(f(f(x))))"
      areTermsJoinable mu cstrs 5 t1 t2 `shouldBe'` Just True
    it "different normal forms" $ do
      let mu = replacementMapFromList [("f", [0, 1]), ("g", [0]), ("h", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(a, a)", "g(x)"), ("g(x)", "a"), ("h(x)", "b")]
      let t1 = termFromString ["x"] "f(f(x, a), b)"
      let t2 = termFromString ["x"] "f(g(x), a)"
      areTermsJoinable mu cstrs 5 t1 t2 `shouldBe'` Just False
    it "undecidable for depth" $ do
      let mu = replacementMapFromList [("f", [0]), ("g", []), ("a", [])]
      let cstrs = cstrsFromString ["x"] [("f(g(x))", "g(x)"), ("g(x)", "a")]
      let t1 = termFromString ["x"] "f(f(f(g(x))))"
      let t2 = termFromString ["x"] "f(g(f(f(x))))"
      areTermsJoinable mu cstrs 1 t1 t2 `shouldBe'` Nothing
  describe "isLHCPInstanceJoinable" $ do
    it "joinable lhcp" $ do
      {- modified example 10 & 24 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b"), ("c(a)", "g(a, a)")]
      let (pair : _) = lhCPs mu cstrs
      isLHCPInstanceJoinable mu cstrs 5 pair (Fun "a" []) (Fun "b" []) `shouldBe'` Just True
    it "unjoinable lhcp" $ do
      {- example 10 & 24 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let (pair : _) = lhCPs mu cstrs
      isLHCPInstanceJoinable mu cstrs 5 pair (Fun "a" []) (Fun "b" []) `shouldBe'` Just False
    it "undecidable lhcp" $ do
      let mu = replacementMapFromList [("f", [0]), ("g", []), ("a", [])]
      let cstrs = cstrsFromString ["x"] [("f(f(x))", "g(x)")]
      let (pair : _) = lhCPs mu cstrs
      let t1 = termFromString ["x"] "f(f(f(f(f(f(f(f(x))))))))"
      let t2 = termFromString ["x"] "g(x)"
      isLHCPInstanceJoinable mu cstrs 1 pair (rename (\x -> (x, 0)) t1) (rename (\x -> (x, 0)) t2) `shouldBe'` Nothing
  describe "isECPJoinable" $ do
    it "joinable mucp" $ do
      {- example 8 [lucas2022] -}
      let mu = replacementMapFromList [("f", []), ("h", []), ("g", [0, 1])]
      let cstrs = cstrsFromString ["x"] [("f(x)", "g(h(x), x)"), ("f(x)", "x"), ("g(x, x)", "x"), ("h(x)", "x")]
      let (pair : _) = extendedCPs mu cstrs
      isECPJoinable mu cstrs 3 pair `shouldBe'` Just True
    it "unjoinable mucp" $ do
      let mu = replacementMapFromList [("g", [0, 1]), ("a", []), ("b", [0])]
      let cstrs = cstrsFromString ["x"] [("g(x, y)", "a"), ("g(x, y)", "b")]
      let (pair : _) = extendedCPs mu cstrs
      isECPJoinable mu cstrs 2 pair `shouldBe'` Just False
    it "undecidable mucp" $ do
      {- example 8 [lucas2022] -}
      let mu = replacementMapFromList [("f", []), ("h", []), ("g", [0, 1])]
      let cstrs = cstrsFromString ["x"] [("f(x)", "g(h(x), x)"), ("f(x)", "x"), ("g(x, x)", "x"), ("h(x)", "x")]
      let (pair : _) = extendedCPs mu cstrs
      isECPJoinable mu cstrs 2 pair `shouldBe'` Nothing
    it "joinable lhcp" $ do
      let mu = replacementMapFromList [("f", [0, 1]), ("g", []), ("a", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, y)", "g(x)"), ("g(x)", "a")]
      let (pair : _) = lhCPs mu cstrs
      isECPJoinable mu cstrs 5 (Right pair) `shouldBe'` Just True
    it "unjoinable lhcp" $ do
      {- example 10 & 24 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let (pair : _) = lhCPs mu cstrs
      isECPJoinable mu cstrs 5 (Right pair) `shouldBe'` Just False

shouldBe' a b = aux a `shouldBe` b

aux (Joinable _ _) = Just True
aux (Counterexample {}) = Just False
aux Undecidable = Nothing