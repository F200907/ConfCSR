{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Data.Rewriting.Mu.ReplacementMap
  ( ReplacementMap,
    expandSymbols,
    appendSymbol,
    positions,
    symbolPositions,
    activePositions,
    activeSubtermPositions,
    frozenPositions,
    activeVariables,
    frozenVariables,
    isLHPositive,
    canonicalReplacementMap,
    prettyMu,
  )
where

import Data.Either (isRight, rights)
import Data.List (intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Rewriting.Pos (Pos)
import Data.Rewriting.Rule (Rule (Rule, lhs), Term (Var))
import Data.Rewriting.Rules (lhss)
import qualified Data.Rewriting.Rules as Rules
import Data.Rewriting.Term (root, subtermAt, vars, withArity)
import qualified Data.Rewriting.Term as Term
import Data.Set (Set)
import qualified Data.Set as Set
import Util (Print (prettyString))

{- Zero-indexed replacement map μ -}
type ReplacementMap f = Map f (Set Int)

prettyMu :: (Print f) => ReplacementMap f -> String
prettyMu mu = "(REPLACEMENT-MAP" ++ Map.foldlWithKey pretty' "\n" mu ++ ")"
  where
    pretty' string symbol arity = string ++ "\t(" ++ prettyString symbol ++ (if null arity then "" else ' ' : set) ++ ")\n"
      where
        set = intercalate ", " (map prettyString $ Set.toList arity)

expandSymbols :: (Ord f) => ReplacementMap f -> [Rule f v] -> ReplacementMap f
expandSymbols mu rules = foldl expandSymbols' mu symbols
  where
    symbols = nub (concatMap (Term.funs . withArity) (lhss rules))

    expandSymbols' :: (Ord f) => ReplacementMap f -> (f, Int) -> ReplacementMap f
    expandSymbols' mu (f, arity) = case Map.lookup f mu of
      Just _ -> mu
      Nothing -> Map.insert f (Set.fromList [0 .. (arity - 1)]) mu

appendSymbol :: (Ord f) => f -> Set Int -> ReplacementMap f -> ReplacementMap f
appendSymbol = Map.insert

subtermAt' :: Term f v -> Pos -> Term f v
subtermAt' t pos = case subtermAt t pos of
  Just t -> t
  Nothing -> error $ "Subterm out of bounds. Tried to get the " ++ show pos ++ "th term"

{- Positions of a term t, zero-indexed
  Pos(t) =  { {ε}                                            if t is variable
            { {ε} ∪ {ip | 1 <= i <= n and p ∈ Pos(t_i)}     if t = f(t_1, ..., t_n)
-}
positions :: Term f v -> Set Pos
positions t = Set.fromList $ positions' $ withArity t
  where
    positions' :: Term (f, Int) v -> [Pos]
    positions' t = case root t of
      Left _ -> [[]]
      Right (_, arity) -> [] : [i : p | i <- [0 .. (arity - 1)], p <- positions' (subtermAt' t [i])]

symbolPositions :: Term f v -> Set Pos
symbolPositions t = Set.filter (isRight . root . subtermAt' t) (positions t)

{-
  Term for testing Pos(t) from lecture notes:
  let t = head $ rights [Term.fromString ["x", "y"] "+(s(+(0,s(x))), +(y, s(x)))"]
  [[],[0],[0,0],[0,0,0],[0,0,1],[0,0,1,0],[1],[1,0],[1,1],[1,1,0]]
-}

activePositions :: (Ord f) => ReplacementMap f -> Term f v -> Set Pos
activePositions mu t = Set.fromList $ activePositions' mu (withArity t)
  where
    lookup' :: (Ord f) => f -> Int -> ReplacementMap f -> [Int]
    lookup' f arity mu = maybe [0 .. (arity - 1)] Set.toList (Map.lookup f mu)

    activePositions' :: (Ord f) => ReplacementMap f -> Term (f, Int) v -> [Pos]
    activePositions' mu t = case root t of
      Left _ -> [[]]
      Right (f, arity) -> [] : [i : p | i <- lookup' f arity mu, p <- activePositions' mu (subtermAt' t [i])]

-- Returns only the positions where the subterm can be found
activeSubtermPositions :: (Ord f, Eq v) => ReplacementMap f -> Term f v -> Term f v -> Set Pos
activeSubtermPositions mu t subterm = Set.filter (\p -> subterm == subtermAt' t p) (activePositions mu t)

frozenPositions :: (Ord f) => ReplacementMap f -> Term f v -> Set Pos
frozenPositions mu t = positions t `Set.difference` activePositions mu t

variables' :: (Eq f, Ord v) => ReplacementMap f -> Term f v -> (ReplacementMap f -> Term f v -> Set Pos) -> Set v
variables' mu t pos = Set.fromList [x | x <- vars t, any (\p -> subtermAt' t p == Var x) (pos mu t)]

activeVariables :: (Ord f, Ord v) => ReplacementMap f -> Term f v -> Set v
activeVariables mu t = variables' mu t activePositions

frozenVariables :: (Ord f, Ord v) => ReplacementMap f -> Term f v -> Set v
frozenVariables mu t = variables' mu t frozenPositions

isLHPositive :: (Ord f, Ord v) => ReplacementMap f -> Rule f v -> Bool
isLHPositive mu rule@(Rule lhs rhs) = Set.null $ activeVariables mu lhs `Set.intersection` (frozenVariables mu lhs `Set.union` frozenVariables mu rhs)

canonicalReplacementMap :: Ord f => [Rule f v] -> ReplacementMap f
canonicalReplacementMap rules = foldr (\(f, arity) -> Map.insert f (Set.fromList $ canon' f arity)) symbols roots
  where
    symbols = Map.fromList $ map (,Set.empty) (Rules.funs rules)
    lhss' = lhss rules
    roots = rights $ map (root . withArity) lhss'

    canon' f arity = filter (\i -> isActiveInRules f [i]) [0 .. (arity - 1)]
    isActiveInRules f i = not $ null [i | l <- lhss', p <- Set.toList $ symbolPositions l, f ~ (l, p), (p ++ i) `Set.member` symbolPositions l]

    (~) :: Eq f => f -> (Term f v, Pos) -> Bool
    (~) f (t, p) = case root $ subtermAt' t p of
      Left _ -> False
      Right f' -> f' == f
