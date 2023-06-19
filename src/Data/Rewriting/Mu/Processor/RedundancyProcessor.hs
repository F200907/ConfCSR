module Data.Rewriting.Mu.Processor.RedundancyProcessor where

import Data.List (nub)
import Data.Rewriting.Mu.Problem.Type (Problem (rules), prettyProblem)
import Data.Rewriting.Mu.Processor.Result (Result (..))
import Data.Rewriting.Mu.Processor.State
  ( ProcessorState (ProcessingState, args, problem),
    defaultState,
  )
import Data.Rewriting.Mu.Processor.Type (Processor)
import Data.Rewriting.Rule (Rule (Rule))
import Util (Print)

redundancyProcessor :: (Print f, Print v, Ord f, Ord v) => Processor f v
redundancyProcessor state = do
  let _args = args state
  let _problem = problem state
  let r = filter (not . isRedundant) (nub $ rules _problem)
  let p' = _problem {rules = r}
  return $ Unknown (defaultState _args p') [prettyProblem p']
  where
    isRedundant (Rule lhs rhs) = lhs == rhs