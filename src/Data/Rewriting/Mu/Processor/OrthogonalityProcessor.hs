module Data.Rewriting.Mu.Processor.OrthogonalityProcessor where

import Data.Rewriting.Mu.CriticalPair (prettyECP)
import Data.Rewriting.Mu.Processor.Result (Result (..))
import Data.Rewriting.Mu.Processor.State
  ( ProcessorState (extendedCriticalPairs, leftLinear),
  )
import Data.Rewriting.Mu.Processor.Type (Processor)
import Util (Print)

orthogonalityProcessor :: (Print f, Show v, Print v) => Processor f v
orthogonalityProcessor state
  | not $ leftLinear state = return $ Unknown state ("Not orthogonal. Not left-linear." : conditialPrettyECPs)
  | not $ null $ extendedCriticalPairs state = return $ Unknown state ("Not orthogonal." : conditialPrettyECPs)
  | otherwise = return $ Confluent True ["Orthogonal. System is left-linear and has no extended mu-critical pairs."]
  where
    prettyECPs = map (\cp -> '\t' : prettyECP cp) (extendedCriticalPairs state)
    conditialPrettyECPs
      | null prettyECPs = []
      | otherwise = "Found extended critical pairs:" : prettyECPs