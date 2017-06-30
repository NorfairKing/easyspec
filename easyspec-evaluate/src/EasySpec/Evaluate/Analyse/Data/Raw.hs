{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Data.Raw
    ( rawDataRule
    , rawDataRules
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Files

rawDataRule :: String
rawDataRule = "raw-data"

rawDataRules :: Resource -> Rules ()
rawDataRules ghciResource = do
    tups <- mapM (uncurry $ dataRulesForExampleGroup ghciResource) exampleGroups
    let csvFs = map fst tups
    let rest = concatMap snd tups
    combF <- allDataFile
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    perStrats <- mapM dataRulesForStrategy signatureInferenceStrategies
    rawDataRule ~> needP (combF : rest ++ perStrats)

dataRulesForStrategy :: ES.SignatureInferenceStrategy -> Rules (Path Abs File)
dataRulesForStrategy strat = do
    let rule = ES.sigInfStratName strat
    csvF <- dataFilesForStrategy strat
    combF <- dataFileForStrategy strat
    combineCSVFiles @EvaluatorCsvLine combF csvF
    rule ~> needP [combF]
    pure combF

dataRulesForExampleGroup ::
       Resource
    -> String
    -> [ES.InputSpec]
    -> Rules (Path Abs File, [Path Abs File])
dataRulesForExampleGroup ghciResource groupName exs = do
    csvFs <- mapM (dataRulesForExample ghciResource groupName) exs
    combF <- dataFileForExampleGroup groupName
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    combFs <-
        mapM
            (dataRulesForExampleGroupAndStrategy ghciResource groupName exs)
            signatureInferenceStrategies
    let rule = groupName
    rule ~> needP (combF : combFs)
    pure (combF, combFs)

dataRulesForExampleGroupAndStrategy ::
       Resource
    -> String
    -> [ES.InputSpec]
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
dataRulesForExampleGroupAndStrategy _ groupName exs strat = do
    csvFs <-
        fmap concat $
        forM exs $ \example -> do
            names <- namesInSource example
            mapM (\n -> dataFileFor groupName example n strat) names
    combF <- dataFileForExampleGroupAndStrategy groupName strat
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

dataRulesForExample :: Resource -> GroupName -> Example -> Rules (Path Abs File)
dataRulesForExample ghciResource groupName is = do
    names <- liftIO $ namesInSource is
    csvFs <- forM names $ rulesForFileAndName ghciResource groupName is
    combF <- dataFileForExample groupName is
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileAndName ::
       Resource
    -> GroupName
    -> Example
    -> ExampleFunction
    -> Rules (Path Abs File)
rulesForFileAndName ghciResource groupName is name = do
    csvFs <-
        forM signatureInferenceStrategies $
        rulesForFileNameAndStrat ghciResource groupName is name
    combF <- dataFileForExampleAndName groupName is name
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileNameAndStrat ::
       Resource
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules (Path Abs File)
rulesForFileNameAndStrat ghciResource groupName is name infStrat = do
    jsonF <- rawDataFileFor groupName is name infStrat
    jsonF $%> do
        sourceDir <- getEasyspecSourceDir
        let relevantFiles =
                map (sourceDir </>) $ ES.sigInfRelevantSources infStrat
        let absFile = ES.inputSpecAbsFile is
        needP $ absFile : relevantFiles
        ip <-
            withResource ghciResource 1 $ do
                putLoud $
                    unwords
                        [ "Building raw data file"
                        , toFilePath jsonF
                        , "by running 'easyspec-evaluate' on"
                        , toFilePath absFile
                        , "with focus:"
                        , show $ prettyPrint name
                        , "and signature inference strategy:"
                        , show $ ES.sigInfStratName infStrat
                        ]
                liftIO $ getEvaluationInputPoint is name infStrat
        writeJSON jsonF ip
    tups <-
        forM evaluators $ \ev ->
            (,) ev <$> dataFileForGranular groupName is name infStrat ev
    map snd tups $&%> do
        needP [jsonF]
        ip <- readJSON jsonF
        forM_ tups $ \(evaluator, evaluatorCsvFile) -> do
            dependOnEvaluator evaluator
            putLoud $
                unwords
                    [ "Building evaluated data file"
                    , toFilePath evaluatorCsvFile
                    , "by evaluating the results in"
                    , toFilePath jsonF
                    , "with evaluator"
                    , evaluatorName evaluator
                    ]
            writeCSV evaluatorCsvFile $
                [evaluationInputPointCsvLine ip evaluator]
    combF <- dataFileFor groupName is name infStrat
    combineCSVFiles @EvaluatorCsvLine combF $ map snd tups
    pure combF
