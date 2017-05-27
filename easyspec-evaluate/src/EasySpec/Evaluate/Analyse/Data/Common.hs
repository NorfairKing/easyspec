{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import

import System.FilePath (dropExtensions)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Types ()
import EasySpec.Evaluate.Analyse.Hackage
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Utils

examples :: MonadIO m => m [ES.InputSpec]
examples = do
    cacheF <- (</> $(mkRelFile "examples-cache.json")) <$> tmpDir
    exists <- doesFileExist cacheF
    if exists
        then readJSON cacheF
        else do
            res <- concat <$> mapM packageExamples hackagePackages
            writeJSON cacheF res
            pure res

contrivedExamples :: MonadIO m => m [ES.InputSpec]
contrivedExamples = do
    edir <- examplesDir
    ss <- sourcesIn edir
    pure $ map (ES.InputSpec edir) ss

signatureInferenceStrategies :: [ES.SignatureInferenceStrategy]
signatureInferenceStrategies = ES.inferenceStrategies
