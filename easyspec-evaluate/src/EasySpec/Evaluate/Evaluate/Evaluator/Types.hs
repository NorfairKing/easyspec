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
    , evaluatorIndication :: Indication
    , evaluatorRelevantFiles :: [Path Rel File]
    }

data EvaluationInput = EvaluationInput
    { eiScope :: [ES.EasyId]
    , eiFocusFuncName :: ES.EasyName
    , eiDiscoveredEqs :: [ES.EasyEq]
    , eiRuntime :: Double
    } deriving (Show, Eq)

data Indication
    = GreaterIsBetter
    | SmallerIsBetter
    deriving (Show, Eq)

prettyIndication :: Indication -> String
prettyIndication GreaterIsBetter = "Greater is better."
prettyIndication SmallerIsBetter = "Smaller is better."
