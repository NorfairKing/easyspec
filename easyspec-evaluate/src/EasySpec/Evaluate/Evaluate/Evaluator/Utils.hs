module EasySpec.Evaluate.Evaluate.Evaluator.Utils
    ( isRelevantEquation
    , relevantEquations
    ) where

import Import

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types

isRelevantEquation :: EvaluationInput -> ES.EasyEq -> Bool
isRelevantEquation ei = ES.mentionsEq $ eiFocusFuncName ei

relevantEquations :: EvaluationInput -> [ES.EasyEq]
relevantEquations ei = filter (isRelevantEquation ei) (eiDiscoveredEqs ei)
