{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.RelevantEquations
    ( relevantEquationsEvaluator
    ) where

import Import

import qualified EasySpec.Discover as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types

relevantEquationsEvaluator :: Evaluator
relevantEquationsEvaluator =
    Evaluator
    { evaluatorName = "relevant-equations"
    , evaluatorGather = Just . genericLength . go
    , evaluatorPretty = \ei -> unwords [show . length . go $ ei, "equations"]
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    , evaluatorRelevantFiles = [$(mkRelFile __FILE__)]
    }
  where
    go ei = filter (ES.mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)
