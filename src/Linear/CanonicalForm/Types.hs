-- |
-- Module: Linear.Simplex.CanonicalForm.Types
-- Description: Types for augmented (slack) form of linear programming problems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.CanonicalForm.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Linear.Constraint.Linear.Types (LinearEquation (..))
import Linear.Expr.Types (Expr, ExprVarsOnly)
import Linear.Expr.Util (exprVarsOnlyVars)
import Linear.System.Linear.Types (LinearSystem (..))
import Linear.System.Simple.Types
import Linear.Var.Types (SimplexNum, Var)

-- https://en.wikipedia.org/wiki/Linear_programming#Augmented_form_(slack_form)
data CanonicalForm = CanonicalForm
  { constraints :: !LinearSystem
  , originalVars :: !(Set.Set Var)
  , systemVars :: !(Set.Set Var)
  , systemSlackVars :: !(Set.Set Var) -- all vars are non-negative
  , eliminatedVarsMap :: !(Map.Map Var Expr)
  }
  deriving (Show, Eq, Read, Generic)
