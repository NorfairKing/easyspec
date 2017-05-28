{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import

import System.FilePath (dropExtensions)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common.TH
import EasySpec.Evaluate.Analyse.Hackage
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types ()
import EasySpec.Utils

examples :: [ES.InputSpec]
examples = $(buildExamples)

namesInSource :: (MonadIO m, MonadMask m) => ES.InputSpec -> m [ES.EasyName]
namesInSource is = do
    let cache = $(makeExampleCache)
    case lookup is cache of
        Nothing -> findNamesInSource is
        Just r -> pure r
