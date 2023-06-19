module Data.Rewriting.Mu.Rewrite where

import Data.Either (lefts, rights)
import Data.List (nub)
import Data.Rewriting.Mu.ReplacementMap
import Data.Rewriting.Rule (Rule)
import Data.Rewriting.Rules (Reduct (..), Strategy)
import qualified Data.Rewriting.Rules as Rules
import Data.Rewriting.Term (Term)
import qualified Data.Set as Set

{- NOTE: this is quite the inefficient solution, maybe we could optimize it by not considering frozen positions at all -}
rewrite :: Ord f => ReplacementMap f -> Strategy f v v' -> Strategy f v v'
rewrite mu r t = filter (\reduct -> pos reduct `Set.member` activePositions mu t) (r t)

fullRewrite :: (Ord f, Ord v', Eq v) => ReplacementMap f -> [Rule f v'] -> Strategy f v v'
fullRewrite mu rules = rewrite mu (Rules.fullRewrite rules)

isNormalForm :: (Ord f, Ord v', Eq v) => ReplacementMap f -> [Rule f v'] -> Term f v -> Bool
isNormalForm mu rules t = null (fullRewrite mu rules t)

{-
  Computes all normal forms for a given term t up to a specified depth n.
  Left results are terms at the depth while right results are true normal forms.
-}
computeNormalForms :: (Ord f, Ord v', Eq v) => ReplacementMap f -> [Rule f v'] -> Term f v -> Int -> [Either (Term f v) (Term f v)]
computeNormalForms mu rules t n
  | isNormalForm mu rules t = [Right t]
  | n <= 0 = [Left t]
  | otherwise = nub $ concatMap (\reduct -> computeNormalForms mu rules (result reduct) (n - 1)) (fullRewrite mu rules t)

{-
  Determines if the term has unique normal forms.
  Nothing corresponds to terms where it's undecidable to determine the normal form.
-}
hasUniqueNormalForm :: (Ord f, Ord v', Eq v) => ReplacementMap f -> [Rule f v'] -> Term f v -> Int -> Maybe Bool
hasUniqueNormalForm mu rules t n
  | length (rights normalForms) > 1 = Just False
  | not $ null $ lefts normalForms = Nothing
  | otherwise = return True
  where
    normalForms = computeNormalForms mu rules t n

branchRewrite :: (Ord f, Ord v', Eq v) => ReplacementMap f -> [Rule f v'] -> Int -> Term f v -> [Term f v]
branchRewrite mu rules = branchRewrite'
  where
    branchRewrite' n t
      | n <= 0 = []
      | otherwise = nub $ foldl (\acc reduct -> branchRewrite' (n - 1) (result reduct) ++ acc) [t] (fullRewrite mu rules t)