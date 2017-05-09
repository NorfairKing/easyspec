{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.Runtime
    ( runtimeEvaluator
    ) where

import Import

import Text.Printf

import EasySpec.Evaluate.Evaluate.Evaluator.Types

runtimeEvaluator :: Evaluator
runtimeEvaluator =
    Evaluator
    { evaluatorName = "runtime"
    , evaluatorGather = Just . eiRuntime
    , evaluatorPretty =
          \ei -> unwords [printf "%.3f" . eiRuntime $ ei, "seconds"]
    , evaluatorUnit = "time"
    , evaluatorQuantity = "second"
    , evaluatorRelevantFiles = [$(mkRelFile __FILE__)]
    }
