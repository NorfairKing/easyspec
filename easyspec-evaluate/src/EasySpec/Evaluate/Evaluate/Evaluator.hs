{-# LANGUAGE CPP #-}

module EasySpec.Evaluate.Evaluate.Evaluator
    ( evaluators
    , dependOnEvaluator
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate.Evaluator.Combinators
import EasySpec.Evaluate.Evaluate.Evaluator.Equations
import EasySpec.Evaluate.Evaluate.Evaluator.RelevantEquations
import EasySpec.Evaluate.Evaluate.Evaluator.Runtime
import EasySpec.Evaluate.Evaluate.Evaluator.Types

dependOnEvaluator :: Evaluator -> Action ()
dependOnEvaluator ev = do
    here <- liftIO getCurrentDir
    needP $ map (here </>) $ evaluatorRelevantFiles ev

evaluators :: [Evaluator]
evaluators = baseEvaluators ++ makeCombinationsOf baseEvaluators

baseEvaluators :: [Evaluator]
baseEvaluators =
    [equationsEvaluator, runtimeEvaluator, relevantEquationsEvaluator]
