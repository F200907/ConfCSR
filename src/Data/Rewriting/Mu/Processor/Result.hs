module Data.Rewriting.Mu.Processor.Result
  ( Result (..),
    glueMessages,
  )
where

import Data.Rewriting.Mu.Processor.State (ProcessorState)

data Result f v = Unknown (ProcessorState f v) [String] | Confluent Bool [String]

instance Show (Result f v) where
  show (Confluent conf proof) = (if conf then "YES" else "NO") ++ "\n\n" ++ unlines proof
  show (Unknown _ proof) = "MAYBE" ++ "\n\n" ++ unlines proof

glueMessages :: Result f v -> [String] -> Result f v
glueMessages (Unknown state msgs) msgs' = Unknown state (msgs' ++ msgs)
glueMessages (Confluent conf msgs) msgs' = Confluent conf (msgs' ++ msgs)