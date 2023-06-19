{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (maybeLeft, maybeRight, Print, prettyString, prettyTerm, prettyRule) where

import Data.List (intercalate)
import Data.Rewriting.Rule (Rule (Rule))
import Data.Rewriting.Term (Term (Fun, Var))

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right b) = Just b
maybeRight _ = Nothing

class Print a where
  prettyString :: a -> String

instance {-# OVERLAPPING #-} Print String where
  prettyString :: String -> String
  prettyString s = s

instance {-# OVERLAPPING #-} Show a => Print a where
  prettyString = show

prettyTerm :: (Print f, Print v) => Term f v -> String
prettyTerm (Var x) = prettyString x
prettyTerm (Fun f fs) = prettyString f ++ "(" ++ intercalate "," (map prettyTerm fs) ++ ")"

prettyRule :: (Print f, Print v) => Rule f v -> String
prettyRule (Rule lhs rhs) = prettyTerm lhs ++ " -> " ++ prettyTerm rhs