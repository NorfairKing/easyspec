{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.ChunksPlus
    ( inferChunksPlus
    ) where

import Import

import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksPlus :: SignatureInferenceStrategy
inferChunksPlus =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus"
    , sigInfRelevantSources = [$(mkRelFile __FILE__)]
    , inferSignature =
          \focus scope' ->
              let scope = scope' \\ focus
                  terminal = (const $ Just $ makeNamedExps focus, 0, [])
                  tups = zip [1..] $ (,) <$> focus <*> scope
                  level1s =
                      map
                          (\(i, (ff, otherFunc)) ->
                               (const $ Just $ makeNamedExps ff, i, []))
                  trips =
                      [(a, b, c) | a <- focus, b <- scope, c <- scope, b < c]
                  level2s = flip map trips $ \(ff, sf1, sf2) ->
                        let sf1ns =  undefined
              in InferredSignature $ terminal : level1s ++ level2s
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs

