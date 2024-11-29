-- |
-- Module      : Comparison.Types
-- Description : Types for constraints in linear programming problems
-- Copyright   : (c) Junaid Rasheed, 2020-2024
-- License     : BSD-3
-- Maintainer  : jrasheed178@gmail.com
-- Stability   : experimental
module Comparison.Types where

import Control.Applicative (liftA2)
import Foreign.C.Types (CBool)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, arbitrary, genericShrink, oneof)

data MixedComparison a b = a :<= b | a :>= b | a :== b
  deriving (Show, Read, Eq, Generic)

instance (Arbitrary a, Arbitrary b) => Arbitrary (MixedComparison a b) where
  arbitrary =
    oneof
      [ liftA2 (:<=) arbitrary arbitrary
      , liftA2 (:>=) arbitrary arbitrary
      , liftA2 (:==) arbitrary arbitrary
      ]

getMixedComparisonLHS :: MixedComparison a b -> a
getMixedComparisonLHS (a :<= _) = a
getMixedComparisonLHS (a :>= _) = a
getMixedComparisonLHS (a :== _) = a

getMixedComparisonRHS :: MixedComparison a b -> b
getMixedComparisonRHS (_ :<= b) = b
getMixedComparisonRHS (_ :>= b) = b
getMixedComparisonRHS (_ :== b) = b

class MixedComparison2 c where
  type LhsType c :: *
  type RhsType c :: *

  lhs :: c -> LhsType c
  rhs :: c -> RhsType c

  (.<=) :: c -> Bool
  (.>=) :: c -> Bool

  (.==) :: c -> Bool
  (.==) c = (.>=) c && (.<=) c

data IntComparison = IntComparison Int Int

instance MixedComparison2 IntComparison where
  type LhsType IntComparison = Int
  type RhsType IntComparison = Int

  lhs (IntComparison l _) = l
  rhs (IntComparison _ r) = r

  (.<=) (IntComparison l r) = l <= r
  (.>=) (IntComparison l r) = l >= r

  (.==) (IntComparison l r) = l == r
