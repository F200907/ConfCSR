{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Data.Rewriting.Mu.CriticalPair
  ( muCPs,
    lhCPs,
    extendedCPs,
    MuCP,
    LHCP (..),
    ExtendedCP,
    prettyECP,
    prettyLHCP,
    prettyMuCP,
  )
where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Rewriting.CriticalPair (CP (left, leftPos, right), cps')
import qualified Data.Rewriting.CriticalPair as CP
import Data.Rewriting.Mu.ReplacementMap
import Data.Rewriting.Pos (Pos)
import Data.Rewriting.Rule (Rule (Rule), Term (Var), rename)
import qualified Data.Rewriting.Rule as Rule
import Data.Rewriting.Term (replaceAt, root, subtermAt)
import qualified Data.Rewriting.Term as T
import Data.Set (Set)
import qualified Data.Set as Set
import Util

type MuCP f v = CP f v v

type MarkedTerm f v = Term f (v, Int)

data LHCP f v = LHCP
  { rule :: Rule f (v, Int),
    pos :: Pos,
    s :: MarkedTerm f v,
    t :: MarkedTerm f v,
    x :: MarkedTerm f v,
    y :: MarkedTerm f v
  }
  deriving (Show)

type ExtendedCP f v = Either (MuCP f v) (LHCP f v)

prettyMuCP :: (Print f, Print v, Show v) => MuCP f v -> String
prettyMuCP muCP = prettyTerm (prettify $ left muCP) ++ "~" ++ prettyTerm (prettify $ right muCP)
  where
    prettify = T.rename (\case Right v -> prettyString v ++ "_1"; Left v -> prettyString v ++ "_0")

prettyLHCP :: (Print f, Print v, Show v) => LHCP f v -> String
prettyLHCP lhCP = prettyTerm (prettify $ s lhCP) ++ "~" ++ prettyTerm (prettify $ t lhCP) ++ " <= (" ++ prettyTerm (prettify $ x lhCP) ++ ")o -> (" ++ prettyTerm (prettify $ y lhCP) ++ ")o"
  where
    prettify = T.rename (\(v, i) -> prettyString v ++ "_" ++ prettyString i)

prettyECP :: (Show v, Print f, Print v) => ExtendedCP f v -> String
prettyECP (Left cp) = prettyMuCP cp
prettyECP (Right cp) = prettyLHCP cp

muCPs :: (Ord f, Ord v) => ReplacementMap f -> [Rule f v] -> [MuCP f v]
muCPs mu rules = filter (\cp -> leftPos cp `elem` activePositions mu (CP.left cp)) (cps' rules)

lhCPs :: (Ord f, Ord v) => ReplacementMap f -> [Rule f v] -> [LHCP f v]
lhCPs mu rules = concatMap (\r -> cps r (variables' r) (positions' r)) lhNegatives
  where
    lhNegatives = filter (not . isLHPositive mu) rules

    variables' (Rule lhs rhs) = frozenVariables mu lhs `Set.union` frozenVariables mu rhs
    positions' (Rule lhs _) x = activeSubtermPositions mu lhs (Var x)

    cps rule vars pos = catMaybes [cp rule p | x <- Set.toList vars, p <- Set.toList $ pos x]

    cp :: Rule f v -> Pos -> Maybe (LHCP f v)
    cp rule pos = do
      let rule'@(Rule l r) = rename (,0) rule
      x <- subtermAt l pos
      var <- maybeLeft (root x)
      let y = Var (fst var, 1)
      s <- replaceAt l pos y
      return (LHCP {rule = rule', pos = pos, s = s, t = r, x = x, y = y})

extendedCPs :: (Ord f, Ord v) => ReplacementMap f -> [Rule f v] -> [ExtendedCP f v]
extendedCPs mu rules = map Left muCPs' ++ map Right lhCPs'
  where
    muCPs' = muCPs mu rules
    lhCPs' = lhCPs mu rules