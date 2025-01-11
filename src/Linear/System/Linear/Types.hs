-- |
-- Module: Linear.System.Linear.Types
-- Description: Types for linear programming problems
-- Copyright: (c) Junaid Rasheed, 2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.System.Linear.Types where

import GHC.Generics (Generic)
import Linear.Constraint.Linear.Types (LinearEquation)
import Linear.Expr.Types (Expr)

-- TODO: name this system of equations or something
-- TODO: OR, should I just get rid of this?
newtype LinearSystem = LinearSystem {unLinearSystem :: [LinearEquation]}
  deriving (Show, Eq, Read, Generic)

{- When would I ever want this? Do I need things to be able to be
able to turn into linear systems? Yes. But do I want other people
to be able to do that? It would be nice, but I think we do that in
the future if people actually want it.
-}
-- class CanBeLinearSystem a where
--   toLinearSystem :: a -> LinearSystem

-- instance CanBeLinearSystem LinearSystem where
--   toLinearSystem = id

-- instance CanBeLinearSystem LinearEquation where
--   toLinearSystem id = LinearSystem [id]
