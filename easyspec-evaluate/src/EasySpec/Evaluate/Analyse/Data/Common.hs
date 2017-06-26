{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common.TH
import EasySpec.Evaluate.Types ()

exampleGroups :: [(String, [ES.InputSpec])]
exampleGroups = $(buildExamples)

examples :: [ES.InputSpec]
examples = concatMap snd exampleGroups

namesInSource :: (MonadIO m, MonadMask m) => ES.InputSpec -> m [ES.EasyQName]
namesInSource is = do
    let cache = $(makeExampleCache)
    case lookup is cache of
        Nothing -> findNamesInSource is
        Just r -> pure r

examplesAndNames :: MonadIO m => m [(ES.InputSpec, ES.EasyQName)]
examplesAndNames =
    fmap concat $
    forM examples $ \example -> do
        names <- liftIO $ namesInSource example
        pure $ (,) example <$> names

examplesNamesAndStrategies ::
       MonadIO m
    => m [(ES.InputSpec, ES.EasyQName, ES.SignatureInferenceStrategy)]
examplesNamesAndStrategies = do
    exns <- examplesAndNames
    pure [(e, n, s) | (e, n) <- exns, s <- signatureInferenceStrategies]
