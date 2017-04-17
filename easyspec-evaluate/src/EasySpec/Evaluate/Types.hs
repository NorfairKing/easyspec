module EasySpec.Evaluate.Types where

import Import

import qualified EasySpec.Discover.Types as ES

data EvaluationInputPoint = EvaluationInputPoint
    { eipFile :: Path Abs File
    , eipFunc :: ES.EasyName
    , eipStrat :: ES.SignatureInferenceStrategy
    , eipDiscoveredEqs :: [ES.EasyEq]
    , eipRuntime :: Double
    } deriving (Show, Eq)

data EvaluationInput = EvaluationInput
    { eiFocusFuncName :: ES.EasyName
    , eiDiscoveredEqs :: [ES.EasyEq]
    , eiRuntime :: Double
    } deriving (Show, Eq)

data Evaluator a = Evaluator
    { evaluatorName :: String
    , evaluatorGather :: EvaluationInput -> a
    , evaluatorPretty :: a -> String
    }
