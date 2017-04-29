{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Types where

import Import

import Data.Csv

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

data EvaluatorCsvLine = EvaluatorCsvLine
    { eclPath :: String
    , eclStratName :: String
    , eclFocusFuncName :: String
    , eclEvaluatorName :: String
    , eclEvaluatorOutput :: String
    } deriving (Show, Eq)

instance FromNamedRecord EvaluatorCsvLine where
    parseNamedRecord r =
        EvaluatorCsvLine <$> r .: "path" <*> r .: "strategy" <*> r .: "focus" <*>
        r .: "evaluator" <*>
        r .: "output"

instance ToNamedRecord EvaluatorCsvLine where
    toNamedRecord EvaluatorCsvLine {..} =
        namedRecord
            [ "path" .= eclPath
            , "strategy" .= eclStratName
            , "focus" .= eclFocusFuncName
            , "evaluator" .= eclEvaluatorName
            , "output" .= eclEvaluatorOutput
            ]

instance DefaultOrdered EvaluatorCsvLine where
    headerOrder _ = header ["path", "strategy", "focus", "evaluator", "output"]
