module Data.Rewriting.Mu.Processor.Type where

import Data.Rewriting.Mu.Processor.Result (Result (Confluent, Unknown), glueMessages)
import Data.Rewriting.Mu.Processor.State (ProcessorState)
import Data.Rewriting.Rules (Reduct (result))

type Processor f v = ProcessorState f v -> IO (Result f v)

process :: IO (Result f v) -> Processor f v -> IO (Result f v)
process result processor = do
  result' <- result
  case result' of
    Unknown state msgs -> do
      r <- processor state
      return (r `glueMessages` msgs)
    confluence@(Confluent _ _) -> return confluence