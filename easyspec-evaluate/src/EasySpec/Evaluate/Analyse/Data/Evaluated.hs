{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Data.Evaluated
    ( evaluatedDataRules
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Files

evaluatedDataRules :: Rules ()
evaluatedDataRules = do
    evaluatedDataRulesAllData
    mapM_ evaluatedDataRulesForGroup groups
    mapM_ evaluatedDataRulesForStrategy signatureInferenceStrategies
    mapM_ evaluatedDataRulesForEvaluator evaluators
    mapM_ (uncurry evaluatedDataRulesForGroupStrategy) $
        (,) <$> groups <*> signatureInferenceStrategies
    mapM_ (uncurry evaluatedDataRulesForGroupEvaluator) $
        (,) <$> groups <*> evaluators
    mapM_ (uncurry evaluatedDataRulesForStrategyEvaluator) $
        (,) <$> signatureInferenceStrategies <*> evaluators
    mapM_ (uncurry3 evaluatedDataRulesForGroupStrategyEvaluator) $
        (,,) <$> groups <*> signatureInferenceStrategies <*> evaluators
    mapM_ (uncurry evaluatedDataRulesForGroupExample) groupsAndExamples
    mapM_ (uncurry3 evaluatedDataRulesForGroupExampleStrategy) $
        lTuple groupsAndExamples signatureInferenceStrategies
    mapM_ (uncurry3 evaluatedDataRulesForGroupExampleEvaluator) $
        lTuple groupsAndExamples evaluators
    mapM_ (uncurry4 evaluatedDataRulesForGroupExampleStrategyEvaluator) $
        lTuple2 groupsAndExamples signatureInferenceStrategies evaluators
    gens <- groupsExamplesAndNames
    mapM_ (uncurry3 evaluatedDataRulesForGroupExampleName) gens
    mapM_ (uncurry4 evaluatedDataRulesForGroupExampleNameStrategy) $
        lTriple gens signatureInferenceStrategies
    mapM_ (uncurry4 evaluatedDataRulesForGroupExampleNameEvaluator) $
        lTriple gens evaluators
    mapM_ (uncurry5 evaluatedDataRulesForGroupExampleNameStrategyEvaluator) $
        lTriple2 gens signatureInferenceStrategies evaluators

evaluatedDataRulesAllData :: Rules ()
evaluatedDataRulesAllData =
    combine allDataFile $ mapM evaluatedFileForGroup groups

evaluatedDataRulesForGroup :: GroupName -> Rules ()
evaluatedDataRulesForGroup groupName =
    combine (evaluatedFileForGroup groupName) $
    mapM (evaluatedFileForGroupStrategy groupName) signatureInferenceStrategies

evaluatedDataRulesForStrategy :: SignatureInferenceStrategy -> Rules ()
evaluatedDataRulesForStrategy strategy =
    combine (evaluatedFileForStrategy strategy) $
    mapM (`evaluatedFileForGroupStrategy` strategy) groups

evaluatedDataRulesForEvaluator :: Evaluator -> Rules ()
evaluatedDataRulesForEvaluator evaluator =
    combine (evaluatedFileForEvaluator evaluator) $
    mapM (`evaluatedFileForGroupEvaluator` evaluator) groups

evaluatedDataRulesForGroupStrategy ::
       GroupName -> SignatureInferenceStrategy -> Rules ()
evaluatedDataRulesForGroupStrategy groupName strategy =
    combine (evaluatedFileForGroupStrategy groupName strategy) $
    mapM (evaluatedFileForGroupStrategyEvaluator groupName strategy) evaluators

evaluatedDataRulesForGroupEvaluator :: GroupName -> Evaluator -> Rules ()
evaluatedDataRulesForGroupEvaluator groupName evaluator =
    combine (evaluatedFileForGroupEvaluator groupName evaluator) $
    mapM
        (\s -> evaluatedFileForGroupStrategyEvaluator groupName s evaluator)
        signatureInferenceStrategies

evaluatedDataRulesForStrategyEvaluator ::
       SignatureInferenceStrategy -> Evaluator -> Rules ()
evaluatedDataRulesForStrategyEvaluator strategy evaluator =
    combine (evaluatedFileForStrategyEvaluator strategy evaluator) $
    mapM
        (\g -> evaluatedFileForGroupStrategyEvaluator g strategy evaluator)
        groups

evaluatedDataRulesForGroupStrategyEvaluator ::
       GroupName -> SignatureInferenceStrategy -> Evaluator -> Rules ()
evaluatedDataRulesForGroupStrategyEvaluator groupName strategy evaluator =
    combine
        (evaluatedFileForGroupStrategyEvaluator groupName strategy evaluator) $
    mapM
        (\n ->
             evaluatedFileForGroupExampleStrategyEvaluator
                 groupName
                 n
                 strategy
                 evaluator)
        (groupExamples groupName)

evaluatedDataRulesForGroupExample :: GroupName -> Example -> Rules ()
evaluatedDataRulesForGroupExample groupName example =
    combine (evaluatedFileForGroupExample groupName example) $
    mapM
        (evaluatedFileForGroupExampleStrategy groupName example)
        signatureInferenceStrategies

evaluatedDataRulesForGroupExampleStrategy ::
       GroupName -> Example -> SignatureInferenceStrategy -> Rules ()
evaluatedDataRulesForGroupExampleStrategy groupName example strategy =
    combine (evaluatedFileForGroupExampleStrategy groupName example strategy) $
    mapM
        (evaluatedFileForGroupExampleStrategyEvaluator
             groupName
             example
             strategy)
        evaluators

evaluatedDataRulesForGroupExampleEvaluator ::
       GroupName -> Example -> Evaluator -> Rules ()
evaluatedDataRulesForGroupExampleEvaluator groupName example evaluator =
    combine (evaluatedFileForGroupExampleEvaluator groupName example evaluator) $
    mapM
        (\s ->
             evaluatedFileForGroupExampleStrategyEvaluator
                 groupName
                 example
                 s
                 evaluator)
        signatureInferenceStrategies

evaluatedDataRulesForGroupExampleStrategyEvaluator ::
       GroupName
    -> Example
    -> SignatureInferenceStrategy
    -> Evaluator
    -> Rules ()
evaluatedDataRulesForGroupExampleStrategyEvaluator groupName example strategy evaluator =
    combine
        (evaluatedFileForGroupExampleStrategyEvaluator
             groupName
             example
             strategy
             evaluator) $ do
        names <- namesInSource example
        mapM
            (\n ->
                 evaluatedFileForGroupExampleNameStrategyEvaluator
                     groupName
                     example
                     n
                     strategy
                     evaluator)
            names

evaluatedDataRulesForGroupExampleName ::
       GroupName -> Example -> ExampleFunction -> Rules ()
evaluatedDataRulesForGroupExampleName groupName example name =
    combine (evaluatedFileForGroupExampleName groupName example name) $
    mapM
        (evaluatedFileForGroupExampleNameStrategy groupName example name)
        signatureInferenceStrategies

evaluatedDataRulesForGroupExampleNameStrategy ::
       GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules ()
evaluatedDataRulesForGroupExampleNameStrategy groupName example name strategy =
    combine
        (evaluatedFileForGroupExampleNameStrategy
             groupName
             example
             name
             strategy) $
    mapM
        (evaluatedFileForGroupExampleNameStrategyEvaluator
             groupName
             example
             name
             strategy)
        evaluators

evaluatedDataRulesForGroupExampleNameEvaluator ::
       GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ()
evaluatedDataRulesForGroupExampleNameEvaluator groupName example name evaluator =
    combine
        (evaluatedFileForGroupExampleNameEvaluator
             groupName
             example
             name
             evaluator) $
    mapM
        (\s ->
             evaluatedFileForGroupExampleNameStrategyEvaluator
                 groupName
                 example
                 name
                 s
                 evaluator)
        signatureInferenceStrategies

evaluatedDataRulesForGroupExampleNameStrategyEvaluator ::
       GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Evaluator
    -> Rules ()
evaluatedDataRulesForGroupExampleNameStrategyEvaluator groupName example name strategy evaluator = do
    csvF <-
        evaluatedFileForGroupExampleNameStrategyEvaluator
            groupName
            example
            name
            strategy
            evaluator
    csvF $%> do
        jsonF <- rawDataFileFor groupName example name strategy
        needP [jsonF]
        ip <- readJSON jsonF
        putLoud $
            unwords
                [ "Building evaluated data file"
                , toFilePath csvF
                , "by evaluating the results in"
                , toFilePath jsonF
                , "with evaluator"
                , evaluatorName evaluator
                ]
        writeCSV csvF [evaluationInputPointCsvLine ip evaluator]

combine :: Rules (Path Abs File) -> Rules [Path Abs File] -> Rules ()
combine combFGen csvFsGen = do
    combF <- combFGen
    csvFs <- csvFsGen
    combineEvaluatedFiles combF csvFs

combineEvaluatedFiles :: Path Abs File -> [Path Abs File] -> Rules ()
combineEvaluatedFiles = combineCSVFiles @EvaluatorCsvLine
