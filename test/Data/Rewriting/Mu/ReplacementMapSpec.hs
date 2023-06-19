module Data.Rewriting.Mu.ReplacementMapSpec where

import qualified Data.Map as Map
import Data.Rewriting.Mu.ReplacementMap (activePositions, activeSubtermPositions, activeVariables, canonicalReplacementMap, expandSymbols, frozenVariables, isLHPositive, positions, symbolPositions)
import qualified Data.Set as Set
import SpecUtil (cstrsFromString, replacementMapFromList, ruleFromString, termFromString)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldNotSatisfy, shouldSatisfy)

spec :: Spec
spec = describe "ReplacementMap" $ do
  describe "expandSymbols" $ do
    it "add missing symbols" $ do
      {- example 1 [lucas2022] -}
      let mu = replacementMapFromList [("if", [0])]
      let cstrs =
            cstrsFromString
              ["x", "y"]
              [ ("p(s(x))", "x"),
                ("+(0, x)", "x"),
                ("+(s(x), y)", "s(+(x, y))"),
                ("*(0, y)", "0"),
                ("*(s(x), y)", "+(y, *(x, y))"),
                ("if(true, x, y)", "x"),
                ("if(false, x, y)", "y"),
                ("zero(0)", "true"),
                ("zero(s(x))", "false"),
                ("fact(x)", "if(zero(x), s(0), *(fact(p(x)), x))")
              ]
      expandSymbols mu cstrs
        `shouldBe` replacementMapFromList
          [ ("if", [0]),
            ("p", [0]),
            ("s", [0]),
            ("+", [0, 1]),
            ("*", [0, 1]),
            ("true", []),
            ("false", []),
            ("0", []),
            ("zero", [0]),
            ("fact", [0])
          ]
  describe "positions" $ do
    it "variable term" $ do
      let t = termFromString ["x"] "x"
      positions t `shouldBe` Set.singleton []
    it "ground term" $ do
      let t = termFromString [] "f(a, b(c))"
      positions t `shouldBe` Set.fromList [[], [0], [1], [1, 0]]
    it "term with functions and variables" $ do
      let t = termFromString ["x"] "f(a, b(x, c))"
      positions t `shouldBe` Set.fromList [[], [0], [1], [1, 0], [1, 1]]
  describe "symbolPositions" $ do
    it "variable term" $ do
      let t = termFromString ["x"] "x"
      symbolPositions t `shouldSatisfy` Set.null
    it "ground term" $ do
      let t = termFromString [] "f(a, b(c))"
      symbolPositions t `shouldBe` Set.fromList [[], [0], [1], [1, 0]]
    it "term with functions and variables" $ do
      let t = termFromString ["x"] "f(x, b(x, c))"
      symbolPositions t `shouldBe` Set.fromList [[], [1], [1, 1]]
  describe "activePositions" $ do
    it "empty replacement map" $ do
      let mu = replacementMapFromList [("f", []), ("g", []), ("h", [])]
      let t = termFromString ["x", "y", "z", "w"] "f(x, g(y), h(z, w))"
      activePositions mu t `shouldBe` Set.singleton []
    it "variable term" $ do
      let mu = replacementMapFromList [("f", [0])]
      let t = termFromString ["x"] "x"
      activePositions mu t `shouldBe` Set.singleton []
    it "positive example" $ do
      let mu = replacementMapFromList [("f", [1]), ("g", [0]), ("h", [1])]
      let t = termFromString ["x", "y", "z", "w"] "f(x, g(y), h(z, w))"
      activePositions mu t `shouldBe` Set.fromList [[], [1], [1, 0]]
  describe "activeSubtermPositions" $ do
    it "empty replacement map" $ do
      let mu = replacementMapFromList [("f", []), ("g", []), ("h", [])]
      let t = termFromString ["x", "y", "z", "w"] "f(x, g(y), h(z, w))"
      let t' = termFromString ["y"] "g(y)"
      activeSubtermPositions mu t t' `shouldSatisfy` Set.null
    it "non-existent subterm" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("g", [0]), ("h", [0, 1])]
      let t = termFromString ["x", "y", "z", "w"] "f(x, g(y), h(z, w))"
      let t' = termFromString ["y"] "h(y, y)"
      activeSubtermPositions mu t t' `shouldSatisfy` Set.null
    it "positive example" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("g", [0]), ("h", [0, 1])]
      let t = termFromString ["x", "y", "z", "w"] "f(g(y), g(y), h(z, w))"
      let t' = termFromString ["y"] "g(y)"
      activeSubtermPositions mu t t' `shouldBe` Set.fromList [[1]]
  describe "activeVariables" $ do
    it "variables only in active positions" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("a", [])]
      let t = termFromString ["x", "y"] "f(a, x, y)"
      activeVariables mu t `shouldBe` Set.fromList ["x", "y"]
    it "variables only in frozen positions" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", [])]
      let t = termFromString ["x", "y"] "f(a, x, y)"
      activeVariables mu t `shouldSatisfy` Set.null
    it "variables in both positions" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("a", [])]
      let t = termFromString ["x", "y", "z"] "f(z, x, y)"
      activeVariables mu t `shouldBe` Set.fromList ["x", "y"]
  describe "frozenVariables" $ do
    it "variables only in active positions" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("a", [])]
      let t = termFromString ["x", "y"] "f(a, x, y)"
      frozenVariables mu t `shouldSatisfy` Set.null
    it "variables only in frozen positions" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", [])]
      let t = termFromString ["x", "y"] "f(a, x, y)"
      frozenVariables mu t `shouldBe` Set.fromList ["x", "y"]
    it "variables in both positions" $ do
      let mu = replacementMapFromList [("f", [1, 2]), ("a", [])]
      let t = termFromString ["x", "y", "z"] "f(z, x, y)"
      frozenVariables mu t `shouldBe` Set.fromList ["z"]
  describe "isLHPositive" $ do
    it "lh-positive rule" $ do
      {- example 8 [lucas2022] -}
      let mu = replacementMapFromList [("f", []), ("g", [0, 1]), ("h", [])]
      let r = ruleFromString ["x"] "f(x)" "g(h(x), x)"
      r `shouldSatisfy` isLHPositive mu
    it "lh-negative rule" $ do
      {- example 9 [lucas2022] -}
      let mu = replacementMapFromList [("c", []), ("g", [0])]
      let r = ruleFromString ["x"] "g(x, a)" "c(x)"
      r `shouldNotSatisfy` isLHPositive mu
  describe "canonicalReplacementMap" $ do
    it "one rule per symbol" $ do
      let cstrs = cstrsFromString ["x", "y"] [("f(a, y)", "g(y, y)"), ("i(x, b)", "h(x)"), ("g(x, a)", "b")]
      let mu = canonicalReplacementMap cstrs
      mu `shouldBe` replacementMapFromList [("f", [0]), ("i", [1]), ("a", []), ("b", []), ("g", [1]), ("h", [])]
    it "multiple rules per symbol" $ do
      let cstrs = cstrsFromString ["x", "y"] [("f(a, y)", "g(y)"), ("f(x, b)", "h(x)"), ("f(h(a), x)", "x")]
      let mu = canonicalReplacementMap cstrs
      mu `shouldBe` replacementMapFromList [("f", [0, 1]), ("a", []), ("b", []), ("g", []), ("h", [])]
