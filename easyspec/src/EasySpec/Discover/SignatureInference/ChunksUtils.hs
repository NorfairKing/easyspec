{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.ChunksUtils where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksFrom :: ([EasyId] -> [EasyId] -> [EasyId]) -> SignatureInferenceStrategy
inferChunksFrom scopeMod =
    SignatureInferenceStrategy
    { sigInfStratName = undefined
    , sigInfRelevantSources = [$(mkRelFile __FILE__)]
    , inferSignature =
          \focus scope' ->
              let scope = scopeMod focus scope'
                  terminal = (const $ Just $ makeNamedExps focus, 0, [])
                  tups = zip [1 ..] $ (,) <$> focus <*> scope
                  level1s =
                      flip map tups $ \(i, (ff, otherFunc)) ->
                          (const $ Just $ makeNamedExps [ff, otherFunc], i, [])
              in InferredSignature $ terminal : level1s
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
