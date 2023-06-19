module Data.Rewriting.Mu.Processor.ConfluenceProcessorSpec where

import Arguments (Arguments (Arguments))
import Data.Rewriting.Mu.Problem.Type (Problem (Problem))
import Data.Rewriting.Mu.Processor.ConfluenceProcessor (confluenceProcessor)
import Data.Rewriting.Mu.Processor.State (ProcessorState (terminating), defaultState)
import Data.Rewriting.Mu.Termination.TerminationTool (Terminating (No, Yes))
import SpecUtil (cstrsFromString, maybeConfluence, replacementMapFromList)
import Test.Hspec (Spec, describe, it, pending, shouldBe)

spec :: Spec
spec = describe "ConfluenceProcessor" $ do
  it "system without extended cps" $ do
    {- example 14 [lucas2022] -}
    let mu = replacementMapFromList [("c", [0]), ("g", [0]), ("a", []), ("b", [])]
    let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
    let problem = Problem ["x"] ["g", "c", "a", "b"] cstrs mu Nothing
    result <- confluenceProcessor (defaultState (Arguments "" False False 5 Nothing False) problem)
    maybeConfluence result `shouldBe` Nothing
  it "1 unjoinable cps" $ do
    {- example 10 & 24 [lucas2022] -}
    let mu = replacementMapFromList [("c", []), ("g", [0]), ("a", []), ("b", [])]
    let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
    let problem = Problem ["x"] ["g", "c", "a", "b"] cstrs mu Nothing
    result <- confluenceProcessor (defaultState (Arguments "" False False 5 Nothing False) problem)
    maybeConfluence result `shouldBe` Just False
  it "locally confluent non-terminating system" $ do
    {- example 8 [lucas2022] -}
    let mu = replacementMapFromList [("f", []), ("h", []), ("g", [0, 1])]
    let cstrs = cstrsFromString ["x"] [("f(x)", "g(h(x), x)"), ("f(x)", "x"), ("g(x, x)", "x"), ("h(x)", "x")]
    let problem = Problem ["x"] ["f", "g", "h"] cstrs mu Nothing
    let state = (defaultState (Arguments "" False False 5 Nothing False) problem) {terminating = Just No}
    result <- confluenceProcessor state
    maybeConfluence result `shouldBe` Nothing
  it "locally confluent terminating system" $ do
    {- example 8 [lucas2022] -}
    let mu = replacementMapFromList [("f", []), ("h", []), ("g", [0, 1])]
    let cstrs = cstrsFromString ["x"] [("f(x)", "g(h(x), x)"), ("f(x)", "x"), ("g(x, x)", "x"), ("h(x)", "x")]
    let problem = Problem ["x"] ["f", "g", "h"] cstrs mu Nothing
    let state = (defaultState (Arguments "" False False 5 Nothing False) problem) {terminating = Just Yes}
    result <- confluenceProcessor state
    maybeConfluence result `shouldBe` Just True
  it "undecidable system" $ do
    {- COPS #205 -}
    let mu = replacementMapFromList [("max", [0, 1]), ("0", []), ("s", [])]
    let cstrs = cstrsFromString ["x", "y"] [("max(0, y)", "y"), ("max(s(x), s(y))", "s(max(x, y))"), ("max(x, 0)", "x"), ("max(x, y)", "max(y,x)")]
    let problem = Problem ["x", "y"] ["max", "0", "s"] cstrs mu Nothing
    result <- confluenceProcessor (defaultState (Arguments "" False False 5 Nothing False) problem)
    maybeConfluence result `shouldBe` Nothing
