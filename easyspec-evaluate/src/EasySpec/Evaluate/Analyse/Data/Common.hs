{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common where

import Import

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Data.Common.TH
import EasySpec.Evaluate.Types ()

examples :: [ES.InputSpec]
examples = $(buildExamples)

namesInSource :: (MonadIO m, MonadMask m) => ES.InputSpec -> m [ES.EasyName]
namesInSource is = do
    let cache = $(makeExampleCache)
    case lookup is cache of
        Nothing -> findNamesInSource is
        Just r -> pure r
