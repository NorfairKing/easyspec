{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import hiding (group)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common.TH
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

evaluationGroup :: GroupName
evaluationGroup = "evaluation"

runtimeGroup :: GroupName
runtimeGroup = "groupname"

toyGroup :: GroupName
toyGroup = "toy"

groups :: [GroupName]
groups = nub . sort . map fst $ exampleGroups

exampleGroups :: [(GroupName, [ES.InputSpec])]
exampleGroups = $(buildExamples)

groupExamples :: GroupName -> [ES.InputSpec]
groupExamples name = fromMaybe [] $ lookup name exampleGroups

groupsAndExamples :: [(GroupName, ES.InputSpec)]
groupsAndExamples = concatMap (\(gn, iss) -> (,) gn <$> iss) exampleGroups

groupExamplesAndNames ::
       MonadIO m => GroupName -> m [(ES.InputSpec, ES.EasyQName)]
groupExamplesAndNames name =
    fmap concat $
    forM (groupExamples name) $ \example -> do
        names <- liftIO $ namesInSource example
        pure $ (,) example <$> names

groupsExamplesAndNames ::
       MonadIO m => m [(GroupName, ES.InputSpec, ES.EasyQName)]
groupsExamplesAndNames =
    fmap concat $
    forM exampleGroups $ \(group, es) ->
        fmap concat $
        forM es $ \example -> do
            names <- liftIO $ namesInSource example
            pure $ (,,) group example <$> names

groupExamplesNamesAndEvaluators ::
       MonadIO m => m [(GroupName, ES.InputSpec, ES.EasyQName, Evaluator)]
groupExamplesNamesAndEvaluators = do
    trips <- groupsExamplesAndNames
    pure $ concatMap (\(a, b, c) -> (,,,) a b c <$> evaluators) trips

examples :: [ES.InputSpec]
examples = concatMap snd exampleGroups

namesInSource :: MonadIO m => ES.InputSpec -> m [ES.EasyQName]
namesInSource is = do
    let cache = $(makeExampleCache)
    case lookup is cache of
        Nothing -> liftIO $ findNamesInSource is
        Just r -> pure r

examplesAndNames :: MonadIO m => m [(ES.InputSpec, ES.EasyQName)]
examplesAndNames =
    fmap concat $
    forM examples $ \example -> do
        names <- liftIO $ namesInSource example
        pure $ (,) example <$> names

groupsExamplesNamesAndStrategies ::
       MonadIO m
    => m [(GroupName, Example, ExampleFunction, SignatureInferenceStrategy)]
groupsExamplesNamesAndStrategies = do
    exns <- groupsExamplesAndNames
    pure [(g, e, n, s) | (g, e, n) <- exns, s <- signatureInferenceStrategies]

groupExampleEvaluators :: [(GroupName, Example, Evaluator)]
groupExampleEvaluators = lTuple groupsAndExamples evaluators
