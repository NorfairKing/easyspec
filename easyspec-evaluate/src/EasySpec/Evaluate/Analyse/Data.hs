{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Data where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Types
import EasySpec.Utils

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Utils

import Development.Shake
import Development.Shake.Path

absSourceFile :: MonadIO m => Path Rel File -> m (Path Abs File)
absSourceFile f = (</> f) <$> examplesDir

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

dataFileFor ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor f name strat =
    csvDataFileWithComponents f [prettyPrint name, ES.sigInfStratName strat]

dataFileForExampleAndName ::
       MonadIO m => Path Rel File -> ES.EasyName -> m (Path Abs File)
dataFileForExampleAndName f name =
    csvDataFileWithComponents f [prettyPrint name]

csvDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvDataFileWithComponents = fileInDirWithExtensionAndComponents dataDir "csv"

dataFilesForExampleAndName ::
       MonadIO m => Path Rel File -> ES.EasyName -> m [Path Abs File]
dataFilesForExampleAndName file name =
    forM signatureInferenceStrategies $ dataFileFor file name

dataFileForExample :: MonadIO m => Path Rel File -> m (Path Abs File)
dataFileForExample f = csvDataFileWithComponents f []

dataFilesForExample :: MonadIO m => Path Rel File -> m [Path Abs File]
dataFilesForExample file = do
    absSourceF <- absSourceFile file
    names <- namesInSource absSourceF
    fmap concat $ forM names $ dataFilesForExampleAndName file

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = (</> $(mkRelFile "all.csv")) <$> dataDir

forExamples :: MonadIO m => (Path Rel File -> m a) -> m [a]
forExamples func = do
    edir <- examplesDir
    forSourcesIn edir func

dataRules :: Rules (Path Abs File)
dataRules = do
    ghciResource <- newResource "ghci" 1
    csvFs <- forExamples (dataRulesForExample ghciResource)
    combF <- allDataFile
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

dataRulesForExample :: Resource -> Path Rel File -> Rules (Path Abs File)
dataRulesForExample ghciResource sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    csvFs <- forM names $ rulesForFileAndName ghciResource sourceF
    combF <- dataFileForExample sourceF
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileAndName ::
       Resource -> Path Rel File -> ES.EasyName -> Rules (Path Abs File)
rulesForFileAndName ghciResource sourceF name = do
    csvFs <-
        forM signatureInferenceStrategies $
        rulesForFileNameAndStrat ghciResource sourceF name
    combF <- dataFileForExampleAndName sourceF name
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileNameAndStrat ::
       Resource
    -> Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
rulesForFileNameAndStrat ghciResource sourceF name infStrat = do
    absSourceF <- absSourceFile sourceF
    csvF <- dataFileFor sourceF name infStrat
    csvF $%> do
        needP [absSourceF]
        ip <-
            withResource ghciResource 1 $ do
                putLoud $
                    unwords
                        [ "Building data file"
                        , toFilePath csvF
                        , "by running 'easyspec-evaluate' on"
                        , toFilePath absSourceF
                        , "with focus:"
                        , prettyPrint name
                        , "and signature inference strategy:"
                        , ES.sigInfStratName infStrat
                        ]
                liftIO $ getEvaluationInputPoint absSourceF name infStrat
        writeCSV csvF $ evaluationInputPointCsvLines ip
    pure csvF

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
