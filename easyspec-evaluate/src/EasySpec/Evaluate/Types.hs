module EasySpec.Evaluate.Types where

import Import

import qualified EasySpec.Discover.Types as ES

data EvaluationInputPoint = EvaluationInputPoint
    { eipFile :: Path Abs File
    , eipStrat :: ES.SignatureInferenceStrategy
    , eipDiscoveredEqs :: [ES.EasyEq]
    } deriving (Show, Eq)

newtype EvaluationInput = EvaluationInput
    { eiDiscoveredEqs :: [ES.EasyEq]
    } deriving (Show, Eq)

data Evaluator a = Evaluator
    { evaluatorName :: String
    , evaluatorGather :: EvaluationInput -> a
    , evaluatorPretty :: a -> String
    }
