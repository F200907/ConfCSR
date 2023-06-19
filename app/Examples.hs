module Examples where

import Data.Either (rights)
import qualified Data.Map as Map
import Data.Rewriting.Mu.Problem.Type
  ( Problem
      ( Problem,
        comment,
        replacementMap,
        rules,
        symbols,
        variables
      ),
  )
import Data.Rewriting.Rule (Rule (Rule, lhs, rhs), Term (Fun, Var))
import Data.Rewriting.Term (fromString, parse)
import qualified Data.Set as Set

example01 :: Problem String String
example01 =
  Problem
    { variables = ["x", "y"],
      symbols = ["p", "s", "+", "0", "+", "s", "s", "+", "*", "0", "0", "*", "s", "+", "*", "if", "true", "if", "false", "zero", "0", "true", "zero", "s", "false", "fact", "if", "zero", "s", "0", "*", "fact", "p"],
      rules =
        [ Rule {lhs = Fun "p" [Fun "s" [Var "x"]], rhs = Var "x"},
          Rule {lhs = Fun "+" [Fun "0" [], Var "x"], rhs = Var "x"},
          Rule {lhs = Fun "+" [Fun "s" [Var "x"], Var "y"], rhs = Fun "s" [Fun "+" [Var "x", Var "y"]]},
          Rule {lhs = Fun "*" [Fun "0" [], Var "y"], rhs = Fun "0" []},
          Rule {lhs = Fun "*" [Fun "s" [Var "x"], Var "y"], rhs = Fun "+" [Var "y", Fun "*" [Var "x", Var "y"]]},
          Rule {lhs = Fun "if" [Fun "true" [], Var "x", Var "y"], rhs = Var "x"},
          Rule {lhs = Fun "if" [Fun "false" [], Var "x", Var "y"], rhs = Var "y"},
          Rule {lhs = Fun "zero" [Fun "0" []], rhs = Fun "true" []},
          Rule {lhs = Fun "zero" [Fun "s" [Var "x"]], rhs = Fun "false" []},
          Rule {lhs = Fun "fact" [Var "x"], rhs = Fun "if" [Fun "zero" [Var "x"], Fun "s" [Fun "0" []], Fun "*" [Fun "fact" [Fun "p" [Var "x"]], Var "x"]]}
        ],
      comment = Nothing,
      replacementMap = Map.fromList [("if", Set.fromList [0])]
    }

t = head $ rights [fromString ["x"] "if(zero(0), zero(0), x)"]