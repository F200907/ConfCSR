{-# LANGUAGE DeriveDataTypeable #-}

module Arguments where

import System.Console.CmdArgs

data Arguments = Arguments
  { file :: String,
    brief :: Bool,
    canonical :: Bool,
    depth :: Int,
    termination :: Maybe String,
    tpdb :: Bool
  }
  deriving (Show, Data, Typeable)

arguments :: Arguments
arguments =
  Arguments
    { file = def &= typ "CSTRS-FILE" &= argPos 0,
      brief = False &= help "Omit the proof justification (defaults to false)",
      canonical = False &= help "Use the canonical mu-replacement map (defaults to false)",
      depth = 15 &= help "Search depth for joinability (defaults to 15)",
      termination = Nothing &= typ "SHELL" &= help "Shell command for invoking a termination tool. Appends the filepath to the command and expects \"YES\"/\"NO\"/\"MAYBE\" in stdout. If not provided, the system is assumed non-terminating",
      tpdb = False &= help "Check for termination with TPDB format instead of CSTRS (defaults to false)"
    }
    &= noAtExpand
    &= program "confcsr"
    &= summary ("A confluence tool for context-sensitive rewrite systems" ++ "\n" ++ "Version 0.1")