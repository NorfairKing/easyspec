module EasySpec.Evaluate.Analyse.Data.Content where

import Import

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Utils

rawDataFrom ::
       GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Action EvaluationInputPoint
rawDataFrom groupName i n s = do
    dataFile <- rawDataFileFor groupName i n s
    needP [dataFile]
    readJSON dataFile

rawDataFromExampleAndName ::
       GroupName -> Example -> ExampleFunction -> Action [EvaluationInputPoint]
rawDataFromExampleAndName groupName e n =
    rawDataFromWith $
    mapM (rawDataFileFor groupName e n) signatureInferenceStrategies

rawDataFromExample :: GroupName -> Example -> Action [EvaluationInputPoint]
rawDataFromExample groupName e =
    rawDataFromWith $ do
        names <- liftIO $ namesInSource e
        mapM
            (uncurry $ rawDataFileFor groupName e)
            ((,) <$> names <*> signatureInferenceStrategies)

rawDataFromStrategy ::
       ES.SignatureInferenceStrategy -> Action [EvaluationInputPoint]
rawDataFromStrategy s =
    rawDataFromWith $
    fmap concat <$> forM exampleGroups $ \(groupName, exs) ->
        fmap concat $
        forM exs $ \example -> do
            names <- liftIO $ namesInSource example
            mapM (\n -> rawDataFileFor groupName example n s) names

rawDataFromWith :: Action [Path Abs File] -> Action [EvaluationInputPoint]
rawDataFromWith getFilePaths = do
    dataFiles <- getFilePaths
    needP dataFiles
    concat <$> mapM readJSON dataFiles

dataFrom ::
       GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Action [EvaluatorCsvLine]
dataFrom groupName is name strat =
    dataFromWith $ dataFileFor groupName is name strat

dataFromExampleAndName ::
       ES.InputSpec -> ES.EasyQName -> Action [EvaluatorCsvLine]
dataFromExampleAndName is name =
    dataFromWith $ dataFileForExampleAndName is name

dataFromExample :: GroupName -> Example -> Action [EvaluatorCsvLine]
dataFromExample groupName is = dataFromWith $ dataFileForExample groupName is

dataFromAll :: Action [EvaluatorCsvLine]
dataFromAll = dataFromWith allDataFile

dataFromWith :: Action (Path Abs File) -> Action [EvaluatorCsvLine]
dataFromWith getFilePath = do
    dataFile <- getFilePath
    needP [dataFile]
    readCSV dataFile
