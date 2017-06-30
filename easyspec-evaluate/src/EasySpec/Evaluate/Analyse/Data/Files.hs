{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

getEasyspecSourceDir :: MonadIO m => m (Path Abs Dir)
getEasyspecSourceDir = liftIO $ resolveDir' "../easyspec"

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

rawDataFileFor ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
rawDataFileFor = evaluatedFileForGroupExampleNameStrategy

jsonDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
jsonDataFileWithComponents = dataFileWithComponents "json"

dataFileForGranular ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Evaluator
    -> m (Path Abs File)
dataFileForGranular = evaluatedFileForGroupExampleNameStrategyEvaluator

dataFileFor ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor = evaluatedFileForGroupExampleNameStrategy

dataFileForExampleAndName ::
       MonadIO m => GroupName -> Example -> ExampleFunction -> m (Path Abs File)
dataFileForExampleAndName = evaluatedFileForGroupExampleName

dataFilesForGroupExampleAndName ::
       MonadIO m
    => GroupName
    -> ES.InputSpec
    -> ES.EasyQName
    -> m [Path Abs File]
dataFilesForGroupExampleAndName groupName is name =
    forM signatureInferenceStrategies $ dataFileFor groupName is name

dataFileForExample :: MonadIO m => GroupName -> Example -> m (Path Abs File)
dataFileForExample = evaluatedFileForGroupExample

dataFilesForStrategy ::
       MonadIO m => ES.SignatureInferenceStrategy -> m [Path Abs File]
dataFilesForStrategy strat =
    fmap concat $
    forM exampleGroups $ \(groupName, exs) -> do
        fmap concat $
            forM exs $ \ex -> do
                names <- liftIO $ namesInSource ex
                forM names $ \name -> dataFileFor groupName ex name strat

dataFileForStrategy ::
       MonadIO m => ES.SignatureInferenceStrategy -> m (Path Abs File)
dataFileForStrategy = evaluatedFileForStrategy

csvDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvDataFileWithComponents = dataFileWithComponents "csv"

dataFilesForGroupAndExample ::
       MonadIO m => GroupName -> ES.InputSpec -> m [Path Abs File]
dataFilesForGroupAndExample groupName is = do
    names <- liftIO $ namesInSource is
    fmap concat $ forM names $ dataFilesForGroupExampleAndName groupName is

dataFilesForExampleGroupAndStrategy ::
       MonadIO m => String -> ES.SignatureInferenceStrategy -> m [Path Abs File]
dataFilesForExampleGroupAndStrategy groupName s = do
    tups <- groupExamplesAndNames groupName
    mapM (\(is, n) -> dataFileFor groupName is n s) tups

dataFileForExampleGroupAndStrategy ::
       MonadIO m => String -> SignatureInferenceStrategy -> m (Path Abs File)
dataFileForExampleGroupAndStrategy = evaluatedFileForGroupStrategy

dataFilesForExampleGroup :: MonadIO m => String -> m [Path Abs File]
dataFilesForExampleGroup groupName = do
    tups <- groupExamplesAndNames groupName
    concat <$> mapM (uncurry $ dataFilesForGroupExampleAndName groupName) tups

dataFileForExampleGroup :: MonadIO m => GroupName -> m (Path Abs File)
dataFileForExampleGroup = evaluatedFileForGroup

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = evaluatedFileForAllData

dataFileWithComponents ::
       MonadIO m => String -> Path Rel File -> [String] -> m (Path Abs File)
dataFileWithComponents = fileInDirWithExtensionAndComponents dataDir

--
-- Better filenames below here
--
evaluatedFileForAllData :: MonadIO m => m (Path Abs File)
evaluatedFileForAllData = evaluatedCSVFileWithComponents [] ["all-data"]

evaluatedFileForGroup :: MonadIO m => GroupName -> m (Path Abs File)
evaluatedFileForGroup g = evaluatedCSVFileWithComponents ["per-group"] [g]

evaluatedFileForStrategy ::
       MonadIO m => SignatureInferenceStrategy -> m (Path Abs File)
evaluatedFileForStrategy s =
    evaluatedCSVFileWithComponents ["per-strategy"] [strategyName s]

evaluatedFileForEvaluator :: MonadIO m => Evaluator -> m (Path Abs File)
evaluatedFileForEvaluator e =
    evaluatedCSVFileWithComponents ["per-evaluator"] [evaluatorName e]

evaluatedFileForGroupStrategy ::
       MonadIO m => GroupName -> SignatureInferenceStrategy -> m (Path Abs File)
evaluatedFileForGroupStrategy g s =
    evaluatedCSVFileWithComponents ["per-group-strategy", g] [strategyName s]

evaluatedFileForGroupEvaluator ::
       MonadIO m => GroupName -> Evaluator -> m (Path Abs File)
evaluatedFileForGroupEvaluator g e =
    evaluatedCSVFileWithComponents ["per-group-evaluator", g] [evaluatorName e]

evaluatedFileForStrategyEvaluator ::
       MonadIO m => SignatureInferenceStrategy -> Evaluator -> m (Path Abs File)
evaluatedFileForStrategyEvaluator s e =
    evaluatedCSVFileWithComponents
        ["per-strategy-evaluator", strategyName s]
        [evaluatorName e]

evaluatedFileForGroupStrategyEvaluator ::
       MonadIO m
    => GroupName
    -> SignatureInferenceStrategy
    -> Evaluator
    -> m (Path Abs File)
evaluatedFileForGroupStrategyEvaluator g s e =
    evaluatedCSVFileWithComponents
        ["per-group-strategy-evaluator", g, strategyName s]
        [evaluatorName e]

evaluatedFileForGroupExample ::
       MonadIO m => GroupName -> Example -> m (Path Abs File)
evaluatedFileForGroupExample g m =
    evaluatedCSVFileWithComponents ["per-group-example", g] [exampleModule m]

evaluatedFileForGroupExampleStrategy ::
       MonadIO m
    => GroupName
    -> Example
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
evaluatedFileForGroupExampleStrategy g m s =
    evaluatedCSVFileWithComponents
        ["per-group-example-strategy", g, exampleModule m]
        [strategyName s]

evaluatedFileForGroupExampleEvaluator ::
       MonadIO m => GroupName -> Example -> Evaluator -> m (Path Abs File)
evaluatedFileForGroupExampleEvaluator g m e =
    evaluatedCSVFileWithComponents
        ["per-group-example-evaluator", g, exampleModule m]
        [evaluatorName e]

evaluatedFileForGroupExampleStrategyEvaluator ::
       MonadIO m
    => GroupName
    -> Example
    -> SignatureInferenceStrategy
    -> Evaluator
    -> m (Path Abs File)
evaluatedFileForGroupExampleStrategyEvaluator g m s e =
    evaluatedCSVFileWithComponents
        [ "per-group-example-strategy-evaluator"
        , g
        , exampleModule m
        , strategyName s
        ]
        [evaluatorName e]

evaluatedFileForGroupExampleName ::
       MonadIO m => GroupName -> Example -> ExampleFunction -> m (Path Abs File)
evaluatedFileForGroupExampleName g m n =
    evaluatedCSVFileWithComponents [g, exampleModule m] [prettyPrint n]

evaluatedFileForGroupExampleNameStrategy ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
evaluatedFileForGroupExampleNameStrategy g m n s =
    evaluatedCSVFileWithComponents
        ["per-group-example-name-strategy", g, exampleModule m, prettyPrint n]
        [strategyName s]

evaluatedFileForGroupExampleNameEvaluator ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> Evaluator
    -> m (Path Abs File)
evaluatedFileForGroupExampleNameEvaluator g m n e =
    evaluatedCSVFileWithComponents
        ["per-group-example-name-evaluator", g, exampleModule m, prettyPrint n]
        [evaluatorName e]

evaluatedFileForGroupExampleNameStrategyEvaluator ::
       MonadIO m
    => GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Evaluator
    -> m (Path Abs File)
evaluatedFileForGroupExampleNameStrategyEvaluator g m n s e =
    evaluatedCSVFileWithComponents
        [ "per-group-example-name-strategy-evaluator"
        , g
        , exampleModule m
        , prettyPrint n
        , strategyName s
        ]
        [evaluatorName e]

evaluatedCSVFileWithComponents ::
       MonadIO m => [String] -> [String] -> m (Path Abs File)
evaluatedCSVFileWithComponents dirComps fileComps =
    fileWithComponentsAndExtension
        ((</> $(mkRelDir "evaluated")) <$> dataDir)
        dirComps
        fileComps
        "csv"

fileWithComponentsAndExtension ::
       MonadIO m
    => m (Path Abs Dir)
    -> [String]
    -> [String]
    -> String
    -> m (Path Abs File)
fileWithComponentsAndExtension genDir dirComps fileComps ext = do
    pd <- genDir
    let dirStr = intercalate "/" dirComps
    let fileStr = intercalate "-" fileComps
    liftIO $
        resolveFile pd $
        (case dirComps of
             [] -> []
             _ -> dirStr ++ "/") ++
        fileStr ++ "." ++ ext
