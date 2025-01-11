module Linear.System.Types where

import Linear.Constraint.Types (Constraint)

-- TODO: create Sytem type, list of Constraints
newtype System = System {unSystem :: [Constraint]}
  deriving (Show, Eq, Read)
