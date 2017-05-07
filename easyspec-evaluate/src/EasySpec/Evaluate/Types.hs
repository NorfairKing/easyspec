{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Types where

import Import

import Data.Csv

import qualified EasySpec.Discover.Types as ES

type EvaluatorName = String

data EvaluationInputPoint = EvaluationInputPoint
    { eipInputSpec :: ES.InputSpec
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

data Evaluator = Evaluator
    { evaluatorName :: EvaluatorName
    , evaluatorGather :: EvaluationInput -> Double
    , evaluatorPretty :: Double -> String
    }

data EvaluatorCsvLine = EvaluatorCsvLine
    { eclBaseDir :: Path Abs Dir
    , eclFile :: Path Rel File
    , eclStratName :: String
    , eclFocusFuncName :: String
    , eclEvaluatorName :: EvaluatorName
    , eclEvaluatorOutput :: Double
    } deriving (Show, Eq)

instance FromNamedRecord EvaluatorCsvLine where
    parseNamedRecord r =
        EvaluatorCsvLine <$> p parseAbsDir "base-dir" <*> p parseRelFile "file" <*>
        r .: "strategy" <*>
        r .: "focus" <*>
        r .: "evaluator" <*>
        r .: "output"
      where
        p parser key = do
            v <- r .: key
            case parser v of
                Left err ->
                    fail $
                    unwords
                        [ "Parsing of field"
                        , show key
                        , "failed with error:"
                        , show err
                        ]
                Right res -> pure res

instance ToNamedRecord EvaluatorCsvLine where
    toNamedRecord EvaluatorCsvLine {..} =
        namedRecord
            [ "base-dir" .= toFilePath eclBaseDir
            , "file" .= toFilePath eclFile
            , "strategy" .= eclStratName
            , "focus" .= eclFocusFuncName
            , "evaluator" .= eclEvaluatorName
            , "output" .= eclEvaluatorOutput
            ]

instance DefaultOrdered EvaluatorCsvLine where
    headerOrder _ =
        header ["base-dir", "file", "strategy", "focus", "evaluator", "output"]
