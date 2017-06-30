{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import hiding (group)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common.TH
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

groups :: [String]
groups = nub . sort . map fst $ exampleGroups

exampleGroups :: [(String, [ES.InputSpec])]
exampleGroups = $(buildExamples)

groupExamples :: String -> [ES.InputSpec]
groupExamples name = fromMaybe [] $ lookup name exampleGroups

groupsAndExamples :: [(String, ES.InputSpec)]
groupsAndExamples = concatMap (\(gn, iss) -> (,) gn <$> iss) exampleGroups

groupExamplesAndNames :: MonadIO m => String -> m [(ES.InputSpec, ES.EasyQName)]
groupExamplesAndNames name =
    fmap concat $
    forM (groupExamples name) $ \example -> do
        names <- liftIO $ namesInSource example
        pure $ (,) example <$> names

groupsExamplesAndNames :: MonadIO m => m [(String, ES.InputSpec, ES.EasyQName)]
groupsExamplesAndNames =
    fmap concat $
    forM exampleGroups $ \(group, es) ->
        fmap concat $
        forM es $ \example -> do
            names <- liftIO $ namesInSource example
            pure $ (,,) group example <$> names

groupExamplesNamesAndEvaluators ::
       MonadIO m => m [(String, ES.InputSpec, ES.EasyQName, Evaluator)]
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
    => m [(String, Example, ExampleFunction, SignatureInferenceStrategy)]
groupsExamplesNamesAndStrategies = do
    exns <- groupsExamplesAndNames
    pure [(g, e, n, s) | (g, e, n) <- exns, s <- signatureInferenceStrategies]
