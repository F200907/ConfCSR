module Data.Rewriting.Mu.Problem.Parse
  ( fromString,
    parser,
  )
where

{-
  This parser is compliant with CoCo's CSTRS format (http://project-coco.uibk.ac.at/problems/cstrs.php) from March 2023
-}

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Rewriting.Mu.Problem.Type (Problem)
import qualified Data.Rewriting.Mu.Problem.Type as Prob
import Data.Rewriting.Mu.ReplacementMap (ReplacementMap)
import qualified Data.Rewriting.Problem as TRS
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Read (readMaybe)

fromString :: String -> Either ParseError (Problem String String)
fromString = parse parser "(unknown)"

parser :: Parsec String () (Problem String String)
parser = do
  (trsString, mu) <- extractor
  case TRS.fromString trsString of
    Left err -> fail $ show err
    Right trs -> return $ problem trs mu
  where
    problem :: TRS.Problem String String -> ReplacementMap String -> Problem String String
    problem trs mu =
      Prob.Problem
        { Prob.variables = TRS.variables trs,
          Prob.symbols = nub $ TRS.symbols trs,
          Prob.rules = TRS.strictRules $ TRS.rules trs,
          Prob.comment = TRS.comment trs,
          Prob.replacementMap = mu
        }

extractor :: Parsec String () (String, ReplacementMap String)
extractor = do
  prefix <- manyTill anyChar (try (lookAhead $ string "(REPLACEMENT-MAP") <|> (eof >> return ""))
  mu <- try replMap <|> return Map.empty
  postfix <- manyTill anyChar (try eof)
  return (prefix ++ postfix, mu)

replMap :: Parsec String () (ReplacementMap String)
replMap = do
  spaces
  between (string "(REPLACEMENT-MAP" >> spaces) (char ')') cslist

cslist :: Parsec String () (ReplacementMap String)
cslist = do
  (f, n) <- fun
  remaining <- (spaces >> cslist) <|> return Map.empty
  return (Map.singleton f n `Map.union` remaining)

fun :: Parsec String () (String, Set Int)
fun = do
  char '(' >> spaces
  f <- many (noneOf " \t\n()\"\\,|")
  spaces
  n <- intlist
  char ')' >> spaces
  return (f, n)

intlist :: Parsec String () (Set Int)
intlist = do
  spaces
  n <- readMaybe <$> many (oneOf ['0' .. '9'])
  spaces
  remaining <- (char ',' >> spaces >> intlist) <|> return Set.empty
  return (maybe Set.empty (Set.singleton . pred) n `Set.union` remaining)