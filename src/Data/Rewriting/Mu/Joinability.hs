{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Rewriting.Mu.Joinability where

import Data.List (intersectBy)
import qualified Data.Map as Map
import Data.Rewriting.CriticalPair (CP (..))
import Data.Rewriting.Mu.CriticalPair (ExtendedCP, LHCP (..))
import Data.Rewriting.Mu.ReplacementMap (ReplacementMap, appendSymbol)
import Data.Rewriting.Mu.Rewrite (branchRewrite, computeNormalForms, fullRewrite, isNormalForm)
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Rule as Rule
import qualified Data.Rewriting.Rules as Rules
import Data.Rewriting.Rules.Rewrite (Reduct (result))
import Data.Rewriting.Substitution (apply, compose, match, merge)
import Data.Rewriting.Term (Term, rename)
import qualified Data.Rewriting.Term as Term
import qualified Data.Set as Set
import Util (Print (prettyString), prettyTerm)

data Joinability f v = Joinable (Term f v) String | Counterexample (Term f v) (Term f v) String | Undecidable deriving (Eq, Show)

prettifyJoinable :: (Print f, Print v) => Joinability f v -> Joinability String String
prettifyJoinable (Joinable t p) = Joinable (Term.map prettyString prettyString t) p
prettifyJoinable (Counterexample t t' p) = Counterexample (Term.map prettyString prettyString t) (Term.map prettyString prettyString t') p
prettifyJoinable Undecidable = Undecidable

isCounterexample :: Joinability f v -> Bool
isCounterexample (Counterexample {}) = True
isCounterexample _ = False

isJoinable :: Joinability f v -> Bool
isJoinable (Joinable _ _) = True
isJoinable _ = False

areTermsJoinableBy :: (Ord f, Ord v', Ord v, Print f, Print v) => ReplacementMap f -> [Rule f v'] -> Int -> (Term f v -> Term f v -> Bool) -> Term f v -> Term f v -> Joinability f v
areTermsJoinableBy mu rules n comparator t t'
  | comparator t t' = Joinable t "Terms are equal."
  | normalForm t && normalForm t' = Counterexample t t' "Terms are in different normal forms."
  | not $ null $ intersectBy comparator reducts reducts' = Joinable (head $ intersectBy comparator reducts reducts') "Intersection of rewrite graph is non-empty."
  | any normalForm reducts && any normalForm reducts' = Counterexample (firstNormalForm reducts) (firstNormalForm reducts') "Counterexample with different normal forms."
  | otherwise = Undecidable
  where
    rewrite = branchRewrite mu rules n
    normalForm = isNormalForm mu rules

    firstNormalForm r = head $ filter normalForm r

    reducts = rewrite t
    reducts' = rewrite t'

areTermsJoinable :: (Ord f, Ord v', Ord v, Print f, Print v) => ReplacementMap f -> [Rule f v'] -> Int -> Term f v -> Term f v -> Joinability f v
areTermsJoinable mu rules n = areTermsJoinableBy mu rules n (==)

isLHCPInstanceJoinable :: (Ord f, Ord v, Print f, Print v, Show v) => ReplacementMap f -> [Rule f v] -> Int -> LHCP f v -> Term f (v, Int) -> Term f (v, Int) -> Joinability f v
isLHCPInstanceJoinable mu rules n lhcp@(LHCP _ _ s t x y) x' y' = case joinable' of
  Just (Joinable t p) -> Joinable (dropFreshness t) p
  Just (Counterexample t t' p) -> Counterexample (dropFreshness t) (dropFreshness t') p
  _ -> Undecidable
  where
    dropFreshness = rename fst

    joinable' = do
      sigmaX <- match x x'
      sigmaY <- match y y'
      sub <- sigmaX `merge` sigmaY
      let sigma = apply sub
      let reducts = map result $ fullRewrite mu rules (sigma x)
      return $ areTermsJoinable mu rules n (sigma s) (sigma t)

isECPJoinable :: (Print v, Ord f, Ord v, Print f, Print v, Show v, Show f) => ReplacementMap f -> [Rule f v] -> Int -> ExtendedCP f v -> Joinability String String
isECPJoinable mu rules n (Left mucp) = prettifyJoinable $ areTermsJoinable mu rules n l r
  where
    rename' = rename (either id id)
    l = rename' (left mucp)
    r = rename' (right mucp)
isECPJoinable mu rules n (Right lhcp@(LHCP rule@(Rule l r) p s t x y))
  | isCounterexample ruleInstance = case ruleInstance of Counterexample t t' p -> prettifyJoinable $ Counterexample t t' ("Found counterexample for (" ++ prettyFresh x ++ ")o=" ++ prettyFresh l ++ " -> " ++ prettyFresh r ++ "=(" ++ prettyFresh y ++ ")o. " ++ p)
  | isJoinable constantInstance = prettifyJoinable constantInstance
  | isJoinable variableInstance = prettifyJoinable variableInstance
  | otherwise = Undecidable
  where
    joinable' = isLHCPInstanceJoinable mu rules n lhcp
    variableInstance = joinable' x y
    ruleInstance = joinable' l r
    prettyFresh = prettyTerm . rename (\(v, i) -> prettyString v ++ "_" ++ prettyString i)

    constantInstance =
      let rename' = Term.map Left id
          s' = rename' s
          t' = rename' t
          x' = rename' x
          y' = rename' y
          fresh name = Term.Fun (Right name) []
          c = fresh "c"
          d = fresh "d"
          rules' = Rules.map Left id rules ++ [Rule c d]
          mu' = Map.mapKeys Left mu `Map.union` Map.fromList [(Right "c", Set.empty), (Right "d", Set.empty)]
       in case isLHCPInstanceJoinable mu' rules' n (LHCP (Rule.map Left id rule) p s' t' x' y') c d of
            Joinable reduct p -> Joinable (dropConstants (Term.Var "c_1") (Term.Var "d_1") c d reduct) ("Using fresh constants (" ++ prettyFresh x ++ ")o=c_1, (" ++ prettyFresh y ++ ")o=d_1 and rule (c_1 -> d_1): " ++ p)
            _ -> Undecidable

    dropConstants :: (Print f, Eq f, Eq v, Print v) => Term String String -> Term String String -> Term (Either f String) v -> Term (Either f String) v -> Term (Either f String) v -> Term String String
    dropConstants x y c d (Term.Var x') = Term.Var (prettyString x' ++ "_0")
    dropConstants x y c d f@(Term.Fun f' fs)
      | f == c = x
      | f == d = y
      | otherwise = Term.Fun (case f' of Left f' -> prettyString f' ++ "_0") (map (dropConstants x y c d) fs)
