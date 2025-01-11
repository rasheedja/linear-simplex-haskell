module Linear.Simplex.Solver.Types where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import Linear.Expr.Types (ExprVarsOnly)
import Linear.CanonicalForm.Types (CanonicalForm)
import Linear.System.Linear.Types (LinearSystem)
import Linear.Var.Types (SimplexNum, Var)
import System.Posix.Types (CMode)

data OptimisationDirection = Minimize | Maximize
  deriving (Show, Eq, GHC.Generics.Generic)

data Objective = Objective
  { expr :: Linear.Expr.Types.ExprVarsOnly
  , direction :: OptimisationDirection
  }
  deriving (Show, Eq, GHC.Generics.Generic)

-- TODO: Is it useful to include the system in the result?
data Result = Result

-- TODO: Include the canonical form?
data OptimisationResult = OptimisationResult
  { varMap :: Map.Map Var SimplexNum
  , objVal :: SimplexNum
  }
  deriving (Show, Read, Eq, GHC.Generics.Generic)

-- class (CanBeLinearSystem s) => Solver s where
--   solve :: s -> Objective -> Result
class TwoPhaseSolver inputSystem where
  firstPhase :: inputSystem -> Maybe CanonicalForm

  twoPhaseSolve :: inputSystem -> Objective -> Maybe OptimisationResult
  twoPhaseSolve inputSystem obj =
    let mSf = firstPhase inputSystem
    in  case mSf of
          Nothing -> Nothing
          Just sf -> Just $ systemResult $ secondPhase obj sf
    where
      secondPhase :: Objective -> CanonicalForm -> CanonicalForm
      secondPhase = undefined

      -- This will probably be a proper function
      systemResult :: CanonicalForm -> OptimisationResult
      systemResult = undefined

class CanBeStandardForm problem where
  findSolution :: problem -> Maybe CanonicalForm

-- solveStandardForm :: StandardForm -> Objective -> Maybe Result

class LinearSystemProcessor s where
  type System s :: *

data FeasibleSystem = FeasibleSystem
  { varVals :: Map.Map Var SimplexNum
  , system :: LinearSystem
  }

data Model = Model {model :: Map.Map Var SimplexNum}

data SatResult model = Unsat | Sat model

-- s is a system
class (Monad (SatSolverMonad s)) => SatSolver s where
  type SatSolverOptions s :: *
  type SatSolverMonad s :: * -> *

  solve :: SatSolverOptions s -> s -> (SatSolverMonad s) (SatResult Model)

class (Monad (OptSolverMonad s)) => OptSolver s where
  type OptSolverOptions s :: *
  type OptSolverMonad s :: * -> *

  optimise ::
    OptSolverOptions s -> s -> Objective -> (OptSolverMonad s) (SatResult Model)

-- class (CanBeLinearSystem s) => Solver2 s where
--   solve2 :: s -> Objective -> Result
