module Data.Rewriting.Mu.Problem.ParseSpec where

import Control.Exception (ErrorCall (ErrorCall))
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import Data.Rewriting.Mu.Problem.Parse (fromString, parser)
import Data.Rewriting.Mu.Problem.Type (Problem (Problem, comment, replacementMap, rules, symbols, variables))
import SpecUtil (cstrsFromString, replacementMapFromList)
import Test.Hspec (Spec, anyException, describe, it, pending, shouldBe, shouldMatchList, shouldSatisfy, shouldThrow)

spec :: Spec
spec = describe "Parser" $ do
  it "CSTRS" $ do
    {- example 1 [lucas2022] -}
    let string =
          "(VAR x y) \n\
          \ (REPLACEMENT-MAP (if 1)) \n\
          \ (RULES \n\
          \ p(s(x)) -> x\n\
          \ +(0, x) -> x\n\
          \ +(s(x), y) -> s(+(x, y)) \n\
          \ *(0, y) -> 0\n\
          \ *(s(x), y) -> +(y, *(x, y))\n\
          \ if(true, x, y) -> x\n\
          \ if(false, x, y) -> y\n\
          \ zero(0) -> true\n\
          \ zero(s(x)) -> false\n\
          \ fact(x) -> if(zero(x), s(0), *(fact(p(x)), x))\n\
          \ )"
    let parsed = fromString string
    parsed `shouldSatisfy` isRight
    let cstrs = (case parsed of Right cstrs -> cstrs)
    variables cstrs `shouldMatchList` ["x", "y"]
    symbols cstrs `shouldMatchList` ["p", "s", "+", "*", "0", "if", "true", "false", "zero", "fact"]
    rules cstrs `shouldMatchList` cstrsFromString ["x", "y"] [("p(s(x))", "x"), ("+(0, x)", "x"), ("+(s(x), y)", "s(+(x, y))"), ("*(0, y)", "0"), ("*(s(x), y)", "+(y, *(x, y))"), ("if(true, x, y)", "x"), ("if(false, x, y)", "y"), ("zero(0)", "true"), ("zero(s(x))", "false"), ("fact(x)", "if(zero(x), s(0), *(fact(p(x)), x))")]
    replacementMap cstrs `shouldBe` replacementMapFromList [("if", [0])]
    comment cstrs `shouldBe` Nothing
  it "TRS" $ do
    let string =
          "(VAR x y)\n\
          \(STRATEGY CONTEXTSENSITIVE (if 1))\n\
          \(RULES\n\
          \    p(s(x)) -> x\n\
          \    +(0, x) -> x\n\
          \    +(s(x), y) -> s(+(x, y))\n\
          \    *(0, y) -> 0\n\
          \    *(s(x), y) -> +(y, *(x, y))\n\
          \    if(true, x, y) -> x\n\
          \    if(false, x, y) -> y\n\
          \    zero(0) -> true\n\
          \    zero(s(x)) -> false\n\
          \    fact(x) -> if(zero(x), s(0), *(fact(p(x)), x))\n\
          \)"
    let parsed = fromString string
    parsed `shouldSatisfy` isRight
    let cstrs = (case parsed of Right cstrs -> cstrs)
    variables cstrs `shouldMatchList` ["x", "y"]
    symbols cstrs `shouldMatchList` ["p", "s", "+", "*", "0", "if", "true", "false", "zero", "fact"]
    rules cstrs `shouldMatchList` cstrsFromString ["x", "y"] [("p(s(x))", "x"), ("+(0, x)", "x"), ("+(s(x), y)", "s(+(x, y))"), ("*(0, y)", "0"), ("*(s(x), y)", "+(y, *(x, y))"), ("if(true, x, y)", "x"), ("if(false, x, y)", "y"), ("zero(0)", "true"), ("zero(s(x))", "false"), ("fact(x)", "if(zero(x), s(0), *(fact(p(x)), x))")]
    replacementMap cstrs `shouldBe` Map.empty
    comment cstrs `shouldBe` Just "STRATEGY CONTEXTSENSITIVE (if 1)"
  it "illegal format" $ do
    let string =
          "(VAR x y z \n\
          \ (REPLACEMENT-MAP (if 1)) \n\
          \ (RULES \n\
          \ p(s(x)) -> x\n\
          \ +(0, x) -> x\n\
          \ +(s(x), y) -> s(+(x, y)) \n\
          \ *(0, y) -> 0\n\
          \ *(s(x), y) -> +(y, *(x, y))\n\
          \ if(true, x, y) -> x\n\
          \ if(false, x, y) -> y\n\
          \ zero(0) -> true\n\
          \ zero(s(x)) -> false\n\
          \ fact(x) -> if(zero(x), s(0), *(fact(p(x)), x))\n\
          \ )"
    fromString string `shouldSatisfy` isLeft
