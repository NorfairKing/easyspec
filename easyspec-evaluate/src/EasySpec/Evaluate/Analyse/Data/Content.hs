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
       ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> Action EvaluationInputPoint
rawDataFrom i n s = do
    dataFile <- rawDataFileFor i n s
    needP [dataFile]
    readJSON dataFile

rawDataFromExampleAndName ::
       ES.InputSpec -> ES.EasyQName -> Action [EvaluationInputPoint]
rawDataFromExampleAndName e n =
    rawDataFromWith $ mapM (rawDataFileFor e n) signatureInferenceStrategies

rawDataFromExample :: ES.InputSpec -> Action [EvaluationInputPoint]
rawDataFromExample e =
    rawDataFromWith $ do
        names <- liftIO $ namesInSource e
        mapM
            (uncurry $ rawDataFileFor e)
            ((,) <$> names <*> signatureInferenceStrategies)

rawDataFromStrategy ::
       ES.SignatureInferenceStrategy -> Action [EvaluationInputPoint]
rawDataFromStrategy s =
    rawDataFromWith $
    fmap concat <$> forM examples $ \example -> do
        names <- liftIO $ namesInSource example
        mapM (\n -> rawDataFileFor example n s) names

rawDataFromWith :: Action [Path Abs File] -> Action [EvaluationInputPoint]
rawDataFromWith getFilePaths = do
    dataFiles <- getFilePaths
    needP dataFiles
    concat <$> mapM readJSON dataFiles

dataFrom ::
       ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> Action [EvaluatorCsvLine]
dataFrom is name strat = dataFromWith $ dataFileFor is name strat

dataFromExampleAndName ::
       ES.InputSpec -> ES.EasyQName -> Action [EvaluatorCsvLine]
dataFromExampleAndName is name =
    dataFromWith $ dataFileForExampleAndName is name

dataFromExample :: ES.InputSpec -> Action [EvaluatorCsvLine]
dataFromExample is = dataFromWith $ dataFileForExample is

dataFromAll :: Action [EvaluatorCsvLine]
dataFromAll = dataFromWith allDataFile

dataFromWith :: Action (Path Abs File) -> Action [EvaluatorCsvLine]
dataFromWith getFilePath = do
    dataFile <- getFilePath
    needP [dataFile]
    readCSV dataFile
