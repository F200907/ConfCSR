module Data.Rewriting.Mu.Processor.OrthogonalityProcessorSpec where

import Arguments (Arguments (Arguments))
import Data.Rewriting.Mu.Problem.Type (Problem (Problem))
import Data.Rewriting.Mu.Processor.OrthogonalityProcessor (orthogonalityProcessor)
import Data.Rewriting.Mu.Processor.Result (Result (Unknown))
import Data.Rewriting.Mu.Processor.State (ProcessorState (ProcessingState, problem), defaultState)
import SpecUtil (cstrsFromString, maybeConfluence, replacementMapFromList)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn, shouldSatisfy)

spec :: Spec
spec = describe "OrthogonalityProcessor" $ do
  let state = defaultState (Arguments "" False False 5 Nothing False)
  it "non-left linear system" $ do
    let mu = replacementMapFromList [("f", [0, 1])]
    let cstrs = cstrsFromString ["x", "y", "z"] [("f(x, x)", "x"), ("f(x, f(y, z))", "f(f(x, y), z)")]
    let problem = Problem ["x", "y", "z"] ["f"] cstrs mu Nothing
    result <- orthogonalityProcessor $ state problem
    maybeConfluence result `shouldBe` Nothing
  it "system with ecps" $ do
    let mu = replacementMapFromList [("f", [0]), ("g", [0])]
    let cstrs = cstrsFromString ["x"] [("g(f(x), x)", "x"), ("f(x)", "a")]
    let problem = Problem ["x"] ["g", "f", "a"] cstrs mu Nothing
    result <- orthogonalityProcessor $ state problem
    maybeConfluence result `shouldBe` Nothing
  it "mu-orthogonal system" $ do
    let mu = replacementMapFromList [("c", [0]), ("g", [0]), ("a", []), ("b", [])]
    let cstrs = cstrsFromString ["x"] [("g(x, a)", "c(x)"), ("a", "b")]
    let problem = Problem ["x"] ["g", "c", "a", "b"] cstrs mu Nothing
    result <- orthogonalityProcessor $ state problem
    maybeConfluence result `shouldBe` Just True
