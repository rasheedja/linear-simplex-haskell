-- |
-- Module: Linear.Constraint.Simple.TypesSpec
-- Description: Tests for Linear.Constraint.Simple.Types
-- Copyright: (c) Junaid Rasheed, 2020-2024
-- License: BSD-3
-- Maintainer: Junaid Rasheed <jrasheed178@gmail.com>
-- Stability: experimental
module Linear.Constraint.Simple.UtilSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Linear.Constraint.Simple.Util
  ( constraintToSimpleConstraint
  , simpleConstraintVars
  , simplifyCoeff
  , simplifySimpleConstraint
  , substVarSimpleConstraint
  , substVarSimpleConstraintExpr
  )
import Linear.Constraint.Util (constraintVars)
import Linear.Expr.Types (Expr (..), ExprVarsOnly (..))
import Linear.Expr.Util (exprVars, exprVarsOnlyVars)
import Linear.Term.Types (Term (..), TermVarsOnly (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, elements)
import TestUtil
  ( evalConstraint
  , evalExpr
  , evalExprVarsOnly
  , evalSimpleConstraint
  , genVarMap
  )
import Prelude

spec :: Spec
spec = do
  describe "SimpleConstraint" $ do
    prop
      "substVarSimpleConstraintExpr with a constant is the same as evaluating with the variable mapped to the constant"
      $ \simpleConstraint c -> do
        let vars = Set.toList $ simpleConstraintVars simpleConstraint
        var <- elements vars
        let varReplacement = Expr (ConstTerm c : [])
        initialVarMap <- genVarMap vars
        let varMap = Map.insert var c initialVarMap
            substitutedSimpleConstraint = substVarSimpleConstraintExpr var varReplacement simpleConstraint
            substitutedSimpleConstraintEval = evalSimpleConstraint varMap substitutedSimpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
        pure
          $ counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nvar: "
                <> show var
                <> "\nconst: "
                <> show c
                <> "\nvarMap: "
                <> show varMap
                <> "\nvarReplacement: "
                <> show varReplacement
                <> "\nsubstitutedSimpleConstraint: "
                <> show substitutedSimpleConstraint
                <> "\nsubstitutedSimpleConstraintEval: "
                <> show substitutedSimpleConstraintEval
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
            )
          $ substitutedSimpleConstraintEval == simpleConstraintEval
    prop
      "substVarSimpleConstraintExpr with an expr is the same as evaluating with the variable mapped to the expr"
      $ \simpleConstraint exprReplacement -> do
        let vars =
              Set.toList $ simpleConstraintVars simpleConstraint <> exprVars exprReplacement
        var <- elements vars
        initialVarMap <- genVarMap vars
        let exprReplacementEval = evalExpr initialVarMap exprReplacement
            varMap = Map.insert var exprReplacementEval initialVarMap
            substitutedSimpleConstraint = substVarSimpleConstraintExpr var exprReplacement simpleConstraint
            simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
            substitutedSimpleConstraintEval = evalSimpleConstraint initialVarMap substitutedSimpleConstraint
        pure
          $ counterexample
            ( "simpleConstraint: "
                <> show simpleConstraint
                <> "\nvar: "
                <> show var
                <> "\nexprReplacement: "
                <> show exprReplacement
                <> "\ninitialVarMap: "
                <> show initialVarMap
                <> "\nexprReplacementEval: "
                <> show exprReplacementEval
                <> "\nvarMap: "
                <> show varMap
                <> "\nsubstitutedSimpleConstraint: "
                <> show substitutedSimpleConstraint
                <> "\nsimpleConstraintEval: "
                <> show simpleConstraintEval
                <> "\nsubstitutedSimpleConstraintEval: "
                <> show substitutedSimpleConstraintEval
            )
          $ substitutedSimpleConstraintEval == simpleConstraintEval
    prop "constraintToSimpleConstraint leads to the same evaluation" $ \constraint -> do
      let vars = Set.toList $ constraintVars constraint
      varMap <- genVarMap vars
      let simpleConstraint = constraintToSimpleConstraint constraint
          constraintEval = evalConstraint varMap constraint
          simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
      pure
        $ counterexample
          ( "constraint: "
              <> show constraint
              <> "\nsimpleConstraint: "
              <> show simpleConstraint
              <> "\ninitialVarMap: "
              <> show varMap
              <> "\nconstraintEval: "
              <> show constraintEval
              <> "\nsimpleConstraintEval"
              <> show simpleConstraintEval
          )
        $ constraintEval == simpleConstraintEval
    prop "simplifyCoeff leads to the same evaluation" $ \simpleConstraint -> do
      let vars = Set.toList $ simpleConstraintVars simpleConstraint
      varMap <- genVarMap vars
      let simplifiedSimpleConstraint = simplifyCoeff simpleConstraint
          simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
          simplifiedSimpleConstraintEval = evalSimpleConstraint varMap simplifiedSimpleConstraint
      pure
        $ counterexample
          ( "simpleConstraint: "
              <> show simpleConstraint
              <> "\nsimplifiedSimpleConstraint: "
              <> show simplifiedSimpleConstraint
              <> "\ninitialVarMap: "
              <> show varMap
              <> "\nsimpleConstraintEval: "
              <> show simpleConstraintEval
              <> "\nsimplifiedSimpleConstraintEval: "
              <> show simplifiedSimpleConstraintEval
          )
        $ simpleConstraintEval == simplifiedSimpleConstraintEval
    prop "simplifySimpleConstraint leads to the same evaluation" $ \simpleConstraint -> do
      let vars = Set.toList $ simpleConstraintVars simpleConstraint
      varMap <- genVarMap vars
      let simplifiedSimpleConstraint = simplifySimpleConstraint simpleConstraint
          simpleConstraintEval = evalSimpleConstraint varMap simpleConstraint
          simplifiedSimpleConstraintEval = evalSimpleConstraint varMap simplifiedSimpleConstraint
      pure
        $ counterexample
          ( "simpleConstraint: "
              <> show simpleConstraint
              <> "\nsimplifiedSimpleConstraint: "
              <> show simplifiedSimpleConstraint
              <> "\ninitialVarMap: "
              <> show varMap
              <> "\nsimpleConstraintEval: "
              <> show simpleConstraintEval
              <> "\nsimplifiedSimpleConstraintEval: "
              <> show simplifiedSimpleConstraintEval
          )
        $ simpleConstraintEval == simplifiedSimpleConstraintEval