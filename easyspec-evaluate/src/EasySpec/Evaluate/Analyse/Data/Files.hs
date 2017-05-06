{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse.Types as ES

import EasySpec.Evaluate.Evaluate

import EasySpec.Evaluate.Analyse.Common

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

dataFileFor ::
       MonadIO m
    => ES.InputSpec
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor is name strat =
    csvDataFileWithComponents
        (ES.inputSpecFile is)
        [prettyPrint name, ES.sigInfStratName strat]

dataFileForExampleAndName ::
       MonadIO m => ES.InputSpec -> ES.EasyName -> m (Path Abs File)
dataFileForExampleAndName is name =
    csvDataFileWithComponents (ES.inputSpecFile is) [prettyPrint name]

csvDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvDataFileWithComponents = fileInDirWithExtensionAndComponents dataDir "csv"

dataFilesForExampleAndName ::
       MonadIO m => ES.InputSpec -> ES.EasyName -> m [Path Abs File]
dataFilesForExampleAndName is name =
    forM signatureInferenceStrategies $ dataFileFor is name

dataFileForExample :: MonadIO m => ES.InputSpec -> m (Path Abs File)
dataFileForExample is = csvDataFileWithComponents (ES.inputSpecFile is) []

dataFilesForExample :: MonadIO m => ES.InputSpec -> m [Path Abs File]
dataFilesForExample is = do
    names <- namesInSource is
    fmap concat $ forM names $ dataFilesForExampleAndName is

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = (</> $(mkRelFile "all.csv")) <$> dataDir
