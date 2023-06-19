module Data.Rewriting.Mu.Processor.State (ProcessorState (..), defaultState, terminating') where

import Arguments (Arguments)
import qualified Arguments
import Data.Rewriting.Mu.CriticalPair (ExtendedCP, extendedCPs)
import Data.Rewriting.Mu.Problem.Type (Problem (replacementMap, rules))
import Data.Rewriting.Mu.Termination.TerminationTool (Terminating, executeTerminationTool)
import Data.Rewriting.Rule (isLeftLinear)
import Util (Print)

{- Processing state containing all properties. Due to Haskell's lazy execution and evaluation only necessary properties are computed at a step -}

data ProcessorState f v = ProcessingState
  { problem :: Problem f v,
    args :: Arguments,
    leftLinear :: Bool,
    extendedCriticalPairs :: [ExtendedCP f v],
    terminating :: Maybe Terminating,
    depth :: Int
  }

defaultState :: (Ord f, Ord v) => Arguments -> Problem f v -> ProcessorState f v
defaultState args' problem =
  ProcessingState
    { problem = problem,
      args = args',
      leftLinear = all isLeftLinear r,
      extendedCriticalPairs = extendedCPs mu r,
      terminating = Nothing,
      depth = Arguments.depth args'
    }
  where
    r = rules problem
    mu = replacementMap problem

{- Computes the termination result if possible and updates the state. Use this to ensure the computation if possible. -}
terminating' :: (Print f, Print v) => ProcessorState f v -> IO (Terminating, ProcessorState f v)
terminating' state = case terminating state of
  Just x -> return (x, state)
  _ -> do
    terminationResult <- executeTerminationTool (args state) (problem state)
    return (terminationResult, state {terminating = Just terminationResult})