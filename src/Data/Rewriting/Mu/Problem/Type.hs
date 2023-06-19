module Data.Rewriting.Mu.Problem.Type
  ( Problem (..),
    prettyProblem,
  )
where

import Data.List (intercalate)
import Data.Rewriting.Mu.ReplacementMap (ReplacementMap, prettyMu)
import Data.Rewriting.Rule (Rule (Rule))
import Util (Print (prettyString), prettyRule, prettyTerm)

data Problem f v = Problem
  { variables :: [v],
    symbols :: [f],
    rules :: [Rule f v],
    replacementMap :: ReplacementMap f,
    comment :: Maybe String
  }
  deriving (Show)

prettyProblem :: (Print f, Print v) => Problem f v -> String
prettyProblem (Problem variables _ rules replacementMap _) = unlines [prettyVars, prettyMu replacementMap, prettyRules]
  where
    prettyVars = "(VAR " ++ unwords (map prettyString variables) ++ ")"
    prettyRules = "(RULES\n\t" ++ intercalate "\n\t" (map prettyRule rules) ++ "\n)"