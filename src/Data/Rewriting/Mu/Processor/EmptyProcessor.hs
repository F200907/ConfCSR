module Data.Rewriting.Mu.Processor.EmptyProcessor where

import Data.Rewriting.Mu.Processor.Result (Result (Unknown))
import Data.Rewriting.Mu.Processor.Type (Processor)

emptyProcessor :: Processor f v
emptyProcessor state = return $ Unknown state []