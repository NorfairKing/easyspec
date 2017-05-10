{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.RelevantFunctions
    ( relevantFunctionsEvaluator
    ) where

import Import

import qualified EasySpec.Discover.CodeUtils as ES
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
                          mentionsEq (eiFocusFuncName ei) eq &&
                          mentionsEq (ES.idName scopeF) eq)
                     (eiDiscoveredEqs ei))
            (eiScope ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2
