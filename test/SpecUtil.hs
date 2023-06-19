{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module SpecUtil where

import qualified Data.Map as Map
import Data.Rewriting.Mu.Processor.Result (Result (Confluent, Unknown))
import Data.Rewriting.Mu.ReplacementMap (ReplacementMap)
import Data.Rewriting.Rule (Rule (Rule, lhs, rhs), Term (Fun, Var))
import qualified Data.Rewriting.Term as Term
import qualified Data.Set as Set

{- unsafe and should only be used for testing -}
termFromString :: [String] -> String -> Term String String
termFromString variables string = case Term.fromString variables string of Right t -> t

ruleFromString :: [String] -> String -> String -> Rule String String
ruleFromString variables lhs rhs = Rule (termFromString variables lhs) (termFromString variables rhs)

cstrsFromString :: [String] -> [(String, String)] -> [Rule String String]
cstrsFromString variables = map (uncurry (ruleFromString variables))

replacementMapFromList :: [(String, [Int])] -> ReplacementMap String
replacementMapFromList xs = Map.fromList $ map (\(x, y) -> (x, Set.fromList y)) xs

maybeConfluence :: Result f v -> Maybe Bool
maybeConfluence (Unknown _ _) = Nothing
maybeConfluence (Confluent c _) = Just c