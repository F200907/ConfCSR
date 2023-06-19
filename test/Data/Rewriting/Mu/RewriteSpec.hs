module Data.Rewriting.Mu.RewriteSpec where

import Data.Rewriting.Mu.Rewrite (branchRewrite, computeNormalForms, fullRewrite, hasUniqueNormalForm, isNormalForm)
import Data.Rewriting.Rules (Reduct (result))
import SpecUtil (cstrsFromString, replacementMapFromList, termFromString)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldMatchList, shouldNotContain, shouldNotSatisfy, shouldSatisfy)

spec :: Spec
spec = describe "Rewrite" $ do
  it "fullRewrite" $ do
    let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
    let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
    let t = termFromString ["x"] "f(f(a, b), f(a, b))"
    let reducts = map result $ fullRewrite mu cstrs t
    reducts `shouldNotContain` [termFromString [] "f(f(a, b), b)"]
    reducts `shouldMatchList` [termFromString [] "a", termFromString [] "f(b, f(a, b))"]
  describe "isNormalForm" $ do
    it "term with normal form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
      let t = termFromString ["x"] "f(b, f(a, b))"
      t `shouldSatisfy` isNormalForm mu cstrs
    it "term without normal form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
      let t = termFromString ["x"] "f(f(a, b), f(a, b))"
      t `shouldNotSatisfy` isNormalForm mu cstrs
  describe "computeNormalForms" $ do
    it "term with unique normal form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
      let t = termFromString [] "f(f(a, b), f(a, f(a, b)))"
      computeNormalForms mu cstrs t 5 `shouldMatchList` [Right $ termFromString [] "f(b, f(a, f(a, b)))"]
    it "term without normal form" $ do
      let mu = replacementMapFromList [("f", [0])]
      let cstrs = cstrsFromString ["x"] [("f(x)", "f(f(x))")]
      let t = termFromString ["x"] "f(x)"
      computeNormalForms mu cstrs t 5 `shouldMatchList` [Left $ termFromString ["x"] "f(f(f(f(f(f(x))))))"]
    it "term without normal form in depth" $ do
      let mu = replacementMapFromList [("f", [0])]
      let cstrs = cstrsFromString ["x"] [("f(f(x))", "f(x)")]
      let t = termFromString ["x"] "f(f(f(f(x))))"
      computeNormalForms mu cstrs t 2 `shouldMatchList` [Left $ termFromString ["x"] "f(f(x))"]
    it "term with unique normal form and depth normal form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(f(x))", "f(x)"), ("f(f(a))", "b")]
      let t = termFromString ["x"] "f(f(f(f(a))))"
      computeNormalForms mu cstrs t 2 `shouldMatchList` [Left $ termFromString [] "f(f(a))", Right $ termFromString [] "f(b)"]
  describe "hasUniqueNormalForm" $ do
    it "term with unique normal form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
      let t = termFromString [] "f(f(a, b), f(a, f(a, b)))"
      hasUniqueNormalForm mu cstrs t 5 `shouldBe` Just True
    it "term with multiple normal forms" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
      let t = termFromString [] "f(f(a, b), f(a, b))"
      hasUniqueNormalForm mu cstrs t 5 `shouldBe` Just False
    it "term with one unique normal form and a depth form" $ do
      let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
      let cstrs = cstrsFromString ["x"] [("f(f(x))", "f(x)"), ("f(f(a))", "b")]
      let t = termFromString ["x"] "f(f(f(f(a))))"
      hasUniqueNormalForm mu cstrs t 2 `shouldBe` Nothing
  it "branchRewrite" $ do
    let mu = replacementMapFromList [("f", [0]), ("a", []), ("b", [])]
    let cstrs = cstrsFromString ["x"] [("f(x, b)", "b"), ("f(x, x)", "a")]
    let t = termFromString [] "f(f(a, b), b)"
    branchRewrite mu cstrs 3 t `shouldMatchList` map (termFromString []) ["f(f(a, b), b)", "f(b, b)", "b", "a"]
