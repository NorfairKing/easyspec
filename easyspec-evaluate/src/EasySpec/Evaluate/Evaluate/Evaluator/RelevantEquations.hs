
module EasySpec.Evaluate.Evaluate.Evaluator.RelevantEquations
    ( relevantEquationsEvaluator
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Evaluate.Evaluator.Utils

relevantEquationsEvaluator :: Evaluator
relevantEquationsEvaluator =
    Evaluator
    { evaluatorName = "relevant-equations"
    , evaluatorGather = Just . genericLength . relevantEquations
    , evaluatorPretty =
          \ei -> unwords [show . length . relevantEquations $ ei, "equations"]
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equations"
    , evaluatorIndication = GreaterIsBetter
    }
