{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.RelevantEquations
    ( relevantEquationsEvaluator
    ) where

import Import

import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES

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
    go ei = filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2
