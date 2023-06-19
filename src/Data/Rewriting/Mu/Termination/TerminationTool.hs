module Data.Rewriting.Mu.Termination.TerminationTool where

import Arguments (Arguments (file, termination, tpdb))
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Rewriting.Mu.Problem.Type (Problem (replacementMap), prettyProblem)
import Data.Rewriting.Mu.ReplacementMap (prettyMu)
import Data.Text (pack, replace, unpack)
import Data.Text.IO (hPutStr)
import GHC.IO (IO (IO))
import GHC.IO.Exception (IOException (IOError))
import GHC.IO.Handle (hClose, hGetContents, hShow)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((-<.>), (</>))
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Temp (withTempFile)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), readProcess, shell)
import Util (Print)

data Terminating = Yes | No | Maybe deriving (Eq, Show)

executeTerminationTool :: (Print f, Print v) => Arguments -> Problem f v -> IO Terminating
executeTerminationTool args problem = do
  computation <-
    try
      ( case termination args of
          Just cmd ->
            if tpdb args
              then withTempFile "" "temp.trs" (\f h -> do convertToTPDB (file args) problem h; tool cmd f)
              else tool cmd (file args)
          _ -> return Maybe
      ) ::
      IO (Either IOError Terminating)
  case computation of
    Left e -> do
      hPutStrLn stderr "IO error while executing termination tool."
      hPrint stderr e
      return Maybe
    Right t -> return t
  where
    answer ans
      | "YES" `isPrefixOf` ans = Yes
      | "NO" `isPrefixOf` ans = No
      | "MAYBE" `isPrefixOf` ans = Maybe
      | otherwise = Maybe

    convertToTPDB f problem h = do
      original <- readFile f
      let original' = replace' "STRATEGY" "COMMENT" (pack original)
      let repl = prettyMu $ replacementMap problem
      let repl' = replace' "," "" $ replace' "REPLACEMENT-MAP" "STRATEGY CONTEXTSENSITIVE" (pack repl)
      hPutStrLn h (unpack original')
      hPutStr h repl'
      hClose h
      where
        replace' a b = replace (pack a) (pack b)

    tool cmd file = do
      let p = shell (cmd ++ " " ++ file)
      (exitCode, stdOut, stdErr) <- readProcess p
      case exitCode of
        ExitSuccess -> return $ answer $ convert stdOut
        ExitFailure _ -> do
          hPutStrLn stderr "Exit error while executing termination tool."
          hPrint stderr stdErr
          return Maybe

    convert byteString = read (show byteString) :: String