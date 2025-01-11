-- |
-- Module: TestUtil
-- Description: Utility functions for testing
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module TestUtil where

import Comparison.Types
  ( MixedComparison ((:<=), (:==), (:>=))
  )
import Control.Monad (forM)
import qualified Data.Map as Map
import Linear.Constraint.Simple.Types (SimpleConstraint (..))
import Linear.Constraint.Types
  ( Constraint (..)
  )
import Linear.Expr.Types (Expr, ExprVarsOnly)
import Linear.Expr.Util (exprToList, exprVarsOnlyToExpr)
import Linear.Simplex.Types (VarLitMap)
import Linear.System.Simple.Types (SimpleSystem (..))
import Linear.Term.Types
  ( Term (..)
  , TermVarsOnly
  )
import Linear.Term.Util (termVarsOnlyToTerm)
import Linear.Var.Types (SimplexNum, Var)
import Test.QuickCheck (Arbitrary (..), Gen)
import Prelude

evalTerm :: VarLitMap -> Linear.Term.Types.Term -> SimplexNum
evalTerm _ (Linear.Term.Types.ConstTerm c) = c
evalTerm varMap (Linear.Term.Types.CoeffTerm c v) =
  c
    * Map.findWithDefault
      (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap)
      v
      varMap
evalTerm varMap (Linear.Term.Types.VarTerm v) =
  Map.findWithDefault
    (error $ "evalTerm: " <> show v <> " not found in varMap " <> show varMap)
    v
    varMap

evalTermVarsOnly :: VarLitMap -> TermVarsOnly -> SimplexNum
evalTermVarsOnly varMap terms = evalTerm varMap $ termVarsOnlyToTerm terms

evalExpr :: VarLitMap -> Expr -> SimplexNum
evalExpr varMap expr = sum $ map (evalTerm varMap) $ exprToList expr

evalExprVarsOnly :: VarLitMap -> ExprVarsOnly -> SimplexNum
evalExprVarsOnly varMap = evalExpr varMap . exprVarsOnlyToExpr

evalConstraint :: VarLitMap -> Constraint -> Bool
evalConstraint varMap (Constraint (lhs :<= rhs)) = evalExpr varMap lhs <= evalExpr varMap rhs
evalConstraint varMap (Constraint (lhs :>= rhs)) = evalExpr varMap lhs >= evalExpr varMap rhs
evalConstraint varMap (Constraint (lhs :== rhs)) = evalExpr varMap lhs == evalExpr varMap rhs

evalSimpleConstraint :: VarLitMap -> SimpleConstraint -> Bool
evalSimpleConstraint varMap (SimpleConstraint (lhs :<= rhs)) = evalExprVarsOnly varMap lhs <= rhs
evalSimpleConstraint varMap (SimpleConstraint (lhs :>= rhs)) = evalExprVarsOnly varMap lhs >= rhs
evalSimpleConstraint varMap (SimpleConstraint (lhs :== rhs)) = evalExprVarsOnly varMap lhs == rhs

evalSimpleSystem :: VarLitMap -> SimpleSystem -> Bool
evalSimpleSystem varMap = all (evalSimpleConstraint varMap) . unSimpleSystem

genVarMap :: [Var] -> Gen VarLitMap
genVarMap vars = do
  varVals <- forM vars $ const arbitrary
  pure $ Map.fromList $ zip vars varVals

isConstExpr :: Expr -> Bool
isConstExpr expr =
  let listExpr = exprToList expr
  in  all
        ( \case
            ConstTerm _ -> True
            _ -> False
        )
        listExpr