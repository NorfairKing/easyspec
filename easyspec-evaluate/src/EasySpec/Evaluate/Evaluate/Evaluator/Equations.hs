{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.Equations
    ( equationsEvaluator
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Types

equationsEvaluator :: Evaluator
equationsEvaluator =
    Evaluator
    { evaluatorName = "equations"
    , evaluatorGather = Just . genericLength . eiDiscoveredEqs
    , evaluatorPretty =
          \ei -> unwords [show . length . eiDiscoveredEqs $ ei, "equations"]
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    , evaluatorRelevantFiles = [$(mkRelFile __FILE__)]
    }
