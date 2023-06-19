module Data.Rewriting.Mu.Processor.ConfluenceProcessor where

import Data.List (find)
import Data.Rewriting.Mu.CriticalPair (prettyECP)
import Data.Rewriting.Mu.Joinability (Joinability (Counterexample, Joinable, Undecidable), isCounterexample, isECPJoinable, isJoinable)
import Data.Rewriting.Mu.Problem.Type (Problem (replacementMap, rules))
import Data.Rewriting.Mu.Processor.Result
import Data.Rewriting.Mu.Processor.State (ProcessorState (depth, extendedCriticalPairs, problem, terminating), terminating')
import Data.Rewriting.Mu.Processor.Type (Processor)
import Data.Rewriting.Mu.Termination.TerminationTool (Terminating (Yes))
import Util (Print (prettyString), prettyTerm)

confluenceProcessor :: (Ord f, Ord v, Show v, Show f, Print v, Print f) => Processor f v
confluenceProcessor state = case find (isCounterexample . snd) joinabilityResults of
  Just (cp, Counterexample t t' p) ->
    return $
      Confluent
        False
        [ "Found counterexample with different normal forms:",
          '\t' : prettyECP cp,
          '\t' : p,
          '\t' : prettyTerm t ++ " =/= " ++ prettyTerm t'
        ]
  Nothing ->
    if all (isJoinable . snd) joinabilityResults
      then do
        let msgs = "All critical pairs are joinable." : concatMap (\(cp, Joinable t p) -> ['\t' : prettyECP cp, '\t' : p, "\t\t" ++ prettyTerm t]) joinabilityResults
        (terminating, state) <- terminating' state
        case terminating of
          Yes -> return $ Confluent True (msgs ++ ["System is terminating."])
          _ -> return $ Unknown state (msgs ++ ["Got termination result: " ++ prettyString terminating])
      else return $ Unknown state ["Extended critical pair which is undecidable:", '\t' : (prettyECP . fst . head) (filter (\(_, j) -> j == Undecidable) joinabilityResults)]
  where
    p = problem state
    mu = replacementMap p
    r = rules p
    n = depth state
    joinabilityResults = map (\cp -> (cp, isECPJoinable mu r n cp)) (extendedCriticalPairs state)