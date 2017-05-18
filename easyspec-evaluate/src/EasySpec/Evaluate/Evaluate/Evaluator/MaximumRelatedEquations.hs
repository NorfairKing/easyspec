{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.MaximumRelatedEquations
    ( maximumRelatedEquationsEvaluator
    ) where

import Import

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Evaluate.Evaluator.Utils

maximumRelatedEquationsEvaluator :: Evaluator
maximumRelatedEquationsEvaluator =
    Evaluator
    { evaluatorName = "maximum-related-equations"
    , evaluatorGather = Just . go
    , evaluatorPretty = \ei -> unwords [show . go @Int $ ei, "equations"]
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    , evaluatorIndication = GreaterIsBetter
    , evaluatorRelevantFiles = [$(mkRelFile __FILE__)]
    }
  where
    go
        :: (Num a, Ord a)
        => EvaluationInput -> a
    go ei =
        case map (genericLength . mentionedFrom (eiScope ei)) $
             relevantEquations ei of
            [] -> 0
            ls -> maximum ls
    mentionedFrom :: [ES.EasyId] -> ES.EasyEq -> [ES.EasyId]
    mentionedFrom ids eq = filter (\i -> ES.mentionsEq (ES.idName i) eq) ids
