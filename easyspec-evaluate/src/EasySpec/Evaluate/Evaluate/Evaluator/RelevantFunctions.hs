{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.RelevantFunctions
    ( relevantFunctionsEvaluator
    ) where

import Import

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types

relevantFunctionsEvaluator :: Evaluator
relevantFunctionsEvaluator =
    Evaluator
    { evaluatorName = "relevant-functions"
    , evaluatorGather = Just . genericLength . go
    , evaluatorPretty = \ei -> unwords [show . length . go $ ei, "functions"]
    , evaluatorUnit = "#"
    , evaluatorQuantity = "function"
    , evaluatorIndication = GreaterIsBetter
    , evaluatorRelevantFiles = [$(mkRelFile __FILE__)]
    }
  where
    go ei
        -- A function is relevant according to a signature inference strategy,
        -- if any property is discovered that both contains that function and
        -- a focus function.
     =
        filter
            (\scopeF ->
                 any
                     (\eq ->
                          ES.mentionsEq (eiFocusFuncName ei) eq &&
                          ES.mentionsEq (ES.idName scopeF) eq)
                     (eiDiscoveredEqs ei))
            (eiScope ei)
