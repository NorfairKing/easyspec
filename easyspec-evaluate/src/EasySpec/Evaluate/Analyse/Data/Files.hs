{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common

getEasyspecSourceDir :: MonadIO m => m (Path Abs Dir)
getEasyspecSourceDir = liftIO $ resolveDir' "../easyspec"

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

rawDataFileFor ::
       MonadIO m
    => ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
rawDataFileFor is name strat =
    jsonDataFileWithComponents
        ($(mkRelDir "raw") </> ES.inputSpecFile is)
        [prettyPrint name, ES.sigInfStratName strat]

jsonDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
jsonDataFileWithComponents = dataFileWithComponents "json"

dataFileFor ::
       MonadIO m
    => ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor is name strat =
    csvDataFileWithComponents
        ($(mkRelDir "evaluated") </> ES.inputSpecFile is)
        [prettyPrint name, ES.sigInfStratName strat]

dataFileForExampleAndName ::
       MonadIO m => ES.InputSpec -> ES.EasyQName -> m (Path Abs File)
dataFileForExampleAndName is name =
    csvDataFileWithComponents
        ($(mkRelDir "combined-per-example-per-name") </> ES.inputSpecFile is)
        [prettyPrint name]

dataFilesForExampleAndName ::
       MonadIO m => ES.InputSpec -> ES.EasyQName -> m [Path Abs File]
dataFilesForExampleAndName is name =
    forM signatureInferenceStrategies $ dataFileFor is name

dataFileForExample :: MonadIO m => ES.InputSpec -> m (Path Abs File)
dataFileForExample is =
    csvDataFileWithComponents
        ($(mkRelDir "combined-per-example") </> ES.inputSpecFile is)
        []

dataFilesForStrategy ::
       MonadIO m => ES.SignatureInferenceStrategy -> m [Path Abs File]
dataFilesForStrategy strat =
    fmap concat $
    forM examples $ \is -> do
        names <- liftIO $ namesInSource is
        forM names $ \name -> dataFileFor is name strat

dataFileForStrategy ::
       MonadIO m => ES.SignatureInferenceStrategy -> m (Path Abs File)
dataFileForStrategy strat = do
    fn <- liftIO $ parseRelFile $ ES.sigInfStratName strat
    csvDataFileWithComponents ($(mkRelDir "combined-per-strategy") </> fn) []

csvDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvDataFileWithComponents = dataFileWithComponents "csv"

dataFilesForExample :: MonadIO m => ES.InputSpec -> m [Path Abs File]
dataFilesForExample is = do
    names <- liftIO $ namesInSource is
    fmap concat $ forM names $ dataFilesForExampleAndName is

dataFilesForExampleGroupAndStrategy ::
       MonadIO m => String -> ES.SignatureInferenceStrategy -> m [Path Abs File]
dataFilesForExampleGroupAndStrategy groupName s = do
    tups <- groupExamplesAndNames groupName
    mapM (\(is, n) -> dataFileFor is n s) tups

dataFileForExampleGroupAndStrategy ::
       MonadIO m => String -> ES.SignatureInferenceStrategy -> m (Path Abs File)
dataFileForExampleGroupAndStrategy groupName s =
    csvDataFileWithComponents
        $(mkRelFile "combined-per-group-per-stragety/data")
        [groupName, ES.sigInfStratName s]

dataFilesForExampleGroup :: MonadIO m => String -> m [Path Abs File]
dataFilesForExampleGroup groupName = do
    tups <- groupExamplesAndNames groupName
    concat <$> mapM (uncurry dataFilesForExampleAndName) tups

dataFileForExampleGroup :: MonadIO m => String -> m (Path Abs File)
dataFileForExampleGroup groupName =
    csvDataFileWithComponents
        $(mkRelFile "combined-per-group/group")
        [groupName]

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = (</> $(mkRelFile "evaluated/all.csv")) <$> dataDir

dataFileWithComponents ::
       MonadIO m => String -> Path Rel File -> [String] -> m (Path Abs File)
dataFileWithComponents = fileInDirWithExtensionAndComponents dataDir
