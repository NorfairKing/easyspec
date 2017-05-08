module EasySpec.Evaluate.Evaluate.Evaluator.Types where

import Import

import qualified EasySpec.Discover.Types as ES

type EvaluatorName = String

data Evaluator = Evaluator
    { evaluatorName :: EvaluatorName
    , evaluatorGather :: EvaluationInput -> Maybe Double
    , evaluatorPretty :: EvaluationInput -> String
    , evaluatorUnit :: String
    , evaluatorQuantity :: String
    , evaluatorRelevantFiles :: [Path Rel File]
    }

data EvaluationInput = EvaluationInput
    { eiFocusFuncName :: ES.EasyName
    , eiDiscoveredEqs :: [ES.EasyEq]
    , eiRuntime :: Double
    } deriving (Show, Eq)
