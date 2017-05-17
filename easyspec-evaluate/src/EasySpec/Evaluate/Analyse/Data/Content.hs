module EasySpec.Evaluate.Analyse.Data.Content where

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Utils

dataFrom
    :: ES.InputSpec
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Action [EvaluatorCsvLine]
dataFrom is name strat = do
    dataFile <- dataFileFor is name strat
    needP [dataFile]
    readCSV dataFile

dataFromExampleAndName :: ES.InputSpec
                       -> ES.EasyName
                       -> Action [EvaluatorCsvLine]
dataFromExampleAndName is name = do
    dataFile <- dataFileForExampleAndName is name
    needP [dataFile]
    readCSV dataFile

dataFromExample :: ES.InputSpec -> Action [EvaluatorCsvLine]
dataFromExample is = do
    dataFile <- dataFileForExample is
    needP [dataFile]
    readCSV dataFile

dataFromAll :: Action [EvaluatorCsvLine]
dataFromAll = do
    dataFile <- allDataFile
    needP [dataFile]
    readCSV dataFile
