{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Rewriting.Mu.CriticalPairSpec where

import Data.Rewriting.CriticalPair (CP (left, right))
import Data.Rewriting.Mu.CriticalPair (LHCP (s, t), lhCPs, muCPs, extendedCPs)
import Data.Rewriting.Term (Term, isVariantOf, rename)
import SpecUtil (cstrsFromString, replacementMapFromList, termFromString)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "CriticalPair" $ do
  describe "muCPs" $ do
    it "system without mu-critical pairs" $ do
      {- example 9 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let pairs = muCPs mu cstrs
      pairs `shouldSatisfy` null
    it "system with mu-critical pairs" $ do
      {- example 8 [lucas2022] -}
      let mu = replacementMapFromList [("f", []), ("h", []), ("g", [0, 1])]
      let cstrs = cstrsFromString ["x"] [("f(x)", "g(h(x), x)"), ("f(x)", "x"), ("g(x, x)", "x"), ("h(x)", "x")]
      let pairs@(pair:_) = muCPs mu cstrs
      pairs `shouldSatisfy` not . null
      length pairs `shouldBe` 1
      renameTerm (left pair) `shouldSatisfy` isVariantOf (termFromString ["x"] "g(h(x), x)")
      renameTerm (right pair) `shouldSatisfy` isVariantOf (termFromString ["x"] "x")
  describe "lhCPs" $ do
    it "system without lh-critical pairs" $ do
      {- example 14 [lucas2022] -}
      let mu = replacementMapFromList [("c", [0]), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let pairs = lhCPs mu cstrs
      pairs `shouldSatisfy` null
    it "system with lh-critical pairs" $ do
      {- example 25 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let pairs@(pair:_) = lhCPs mu cstrs
      pairs `shouldSatisfy` not . null
      length pairs `shouldBe` 1
      s pair `shouldSatisfy` isVariantOf (termFromString ["y", "x"] "g(y, a)")
      t pair `shouldSatisfy` isVariantOf (termFromString ["y", "x"] "c(x)")
  describe "extendedCPs" $ do
    it "system without extended critical pairs" $ do
      {- example 37 [lucas2022] -}
      let mu = replacementMapFromList [("c", [0]), ("g", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
      let pairs = extendedCPs mu cstrs
      pairs `shouldSatisfy` null
    it "system with extended critical pairs" $ do
      let mu = replacementMapFromList [("f", [0]), ("g", [0])]
      let cstrs = cstrsFromString ["x"] [("g(f(x), x)", "x"), ("f(x)", "a")]
      let pairs = extendedCPs mu cstrs
      pairs `shouldSatisfy` not . null
      pairs `shouldSatisfy` all (\p -> case p of
        Left muCP -> isVariantOf (left muCP) (termFromString ["x"] "g(a, x)") && isVariantOf (right muCP) (termFromString ["x"] "x")
        Right lhCP -> isVariantOf (s lhCP) (termFromString ["x", "y"] "g(f(y), x)")  && isVariantOf (t lhCP) (termFromString ["x", "y"] "y")
        )

renameTerm :: (Show f, Show v, Show v') => Term f (Either v v') -> Term f String
renameTerm = rename (\x -> case x of Left v -> show v; Right v -> show v)

instance (Show f, Show v, Show v') => Show (CP f v v') where
  show :: (Show f, Show v, Show v') => CP f v v' -> String
  show cp = "CriticalPair <" ++ show (renameTerm (left cp)) ++ ", " ++ show (renameTerm (right cp)) ++ ">"