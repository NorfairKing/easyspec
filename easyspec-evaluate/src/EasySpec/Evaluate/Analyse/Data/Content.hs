module EasySpec.Evaluate.Analyse.Data.Content where

import Import

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Utils

dataFrom ::
       Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Action [EvaluatorCsvLine]
dataFrom file name strat = do
    dataFile <- dataFileFor file name strat
    needP [dataFile]
    readCSV dataFile

dataFromExampleAndName ::
       Path Rel File -> ES.EasyName -> Action [EvaluatorCsvLine]
dataFromExampleAndName file name = do
    dataFile <- dataFileForExampleAndName file name
    needP [dataFile]
    readCSV dataFile

dataFromExample :: Path Rel File -> Action [EvaluatorCsvLine]
dataFromExample file = do
    dataFile <- dataFileForExample file
    needP [dataFile]
    readCSV dataFile

dataFromAll :: Action [EvaluatorCsvLine]
dataFromAll = do
    dataFile <- allDataFile
    needP [dataFile]
    readCSV dataFile
