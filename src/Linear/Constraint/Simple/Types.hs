-- |
-- Module:      Linear.Constraint.Simple.Types
-- Description: Types for simple linear constraints
-- Copyright:   (c) Junaid Rasheed, 2020-2024
-- License:     BSD-3
-- Maintainer:  jrasheed178@gmail.com
-- Stability:   experimental
module Linear.Constraint.Simple.Types where

import Comparison.Types (MixedComparison)
import GHC.Generics (Generic)
import Linear.Expr.Types (ExprVarsOnly)
import Linear.Var.Types (SimplexNum)
import Test.QuickCheck (Arbitrary (..))

newtype SimpleConstraint = SimpleConstraint
  {unSimpleConstraint :: MixedComparison ExprVarsOnly SimplexNum}
  deriving (Show, Eq, Read, Generic)

instance Arbitrary SimpleConstraint where
  arbitrary = SimpleConstraint <$> arbitrary

class CanBeSimpleConstraint a where
  toSimpleConstraint :: a -> SimpleConstraint
