{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate

import EasySpec.Evaluate.Analyse.Common

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
    absSourceF <- absExampleFile file
    names <- namesInSource absSourceF
    fmap concat $ forM names $ dataFilesForExampleAndName file

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = (</> $(mkRelFile "all.csv")) <$> dataDir
