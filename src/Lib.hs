module Lib where

import Arguments
import Control.Monad (when)
import Data.List (isInfixOf)
import Data.Rewriting.Mu.Problem.Parse (fromString)
import Data.Rewriting.Mu.Problem.Type (Problem (Problem, comment, replacementMap))
import Data.Rewriting.Mu.Processor.ConfluenceProcessor (confluenceProcessor)
import Data.Rewriting.Mu.Processor.EmptyProcessor (emptyProcessor)
import Data.Rewriting.Mu.Processor.OrthogonalityProcessor (orthogonalityProcessor)
import Data.Rewriting.Mu.Processor.RedundancyProcessor (redundancyProcessor)
import Data.Rewriting.Mu.Processor.Result (Result (Unknown))
import Data.Rewriting.Mu.Processor.State (ProcessorState (problem), defaultState)
import Data.Rewriting.Mu.Processor.Type (Processor, process)
import Data.Rewriting.Mu.ReplacementMap (canonicalReplacementMap, expandSymbols)
import System.Console.CmdArgs (cmdArgs)
import System.Environment (getArgs, getProgName, withArgs)
import Text.Parsec (ParseError)

processors :: [Processor String String]
processors =
  [ emptyProcessor,
    redundancyProcessor,
    orthogonalityProcessor,
    confluenceProcessor
  ]

main :: IO ()
main = do
  args' <- getArgs
  when (null args') (do withArgs ["--help"] (cmdArgs arguments); return ())
  args <- cmdArgs arguments
  input <- readFile $ file args
  ( case fromString input of
      (Left e) -> error $ show e
      (Right p) -> do
        let problem = modify args p
        result <- foldl process (return $ Unknown (defaultState args problem) []) processors
        putStrLn $ output problem args result
    )
  where
    modify :: Arguments -> Problem String String -> Problem String String
    modify args problem@(Problem variables symbols rules replacementMap comment) = problem {replacementMap = replacementMap'}
      where
        replacementMap'
          | canonical args = canonicalReplacementMap rules
          | otherwise = expandSymbols replacementMap rules

    output :: Problem f v -> Arguments -> Result f v -> String
    output problem args
      | isConditional problem = if brief args then const "MAYBE" else const "MAYBE\n\nSystem is conditional"
      | brief args = head . lines . show
      | otherwise = show

    isConditional :: Problem f v -> Bool
    isConditional problem = case comment problem of
      Just s -> "CONDITIONTYPE" `isInfixOf` s
      Nothing -> False