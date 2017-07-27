module EasySpec.Evaluate.Evaluate.Evaluator.Types where

import Import

import Data.Function (on)

import qualified EasySpec.Discover.Types as ES

type EvaluatorName = String

data Evaluator = Evaluator
    { evaluatorName :: EvaluatorName
    , evaluatorGather :: EvaluationInput -> Maybe Double
    , evaluatorPretty :: EvaluationInput -> String
    , evaluatorUnit :: String
    , evaluatorQuantity :: String
    , evaluatorIndication :: Indication
    }

instance Show Evaluator where
    show = evaluatorName

instance Eq Evaluator where
    (==) = (==) `on` evaluatorName

instance Ord Evaluator where
    compare = compare `on` evaluatorName

data EvaluationInput = EvaluationInput
    { eiScope :: [ES.EasyId]
    , eiFocusFuncName :: ES.EasyQName
    , eiDiscoveredEqs :: [ES.EasyEq]
    , eiRuntime :: Double
    } deriving (Show, Eq)

data Indication
    = GreaterIsBetter
    | SmallerIsBetter
    | Input
    deriving (Show, Eq)

prettyIndication :: Indication -> String
prettyIndication GreaterIsBetter = "More is better."
prettyIndication SmallerIsBetter = "Less is better."
prettyIndication Input = "This is a part of the input."
