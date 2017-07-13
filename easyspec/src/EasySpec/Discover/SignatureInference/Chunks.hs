{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.Chunks where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunks :: SignatureInferenceStrategy
inferChunks =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks"
    , sigInfRelevantSources = [$(mkRelFile __FILE__)]
    , inferSignature =
          \focus scope' ->
              let scope = scope' \\ focus
              in InferredSignature $
                 (const $ Just $ makeNamedExps focus, 0, []) :
                 map
                     (\(otherFunc, i) ->
                          (const $ Just $ makeNamedExps focus, i, []))
                     (zip scope [1 ..])
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
