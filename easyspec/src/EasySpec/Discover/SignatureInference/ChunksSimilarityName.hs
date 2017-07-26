{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.ChunksSimilarityName where

import Import

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferChunksSimilarityName :: Int -> SignatureInferenceStrategy
inferChunksSimilarityName i =
    let diff = simDiff $ prettyPrintOneLine . idName
    in (inferChunksFrom $ diffChoice i diff)
       { sigInfStratName = "chunks-similarity-name-" ++ show i
       , sigInfRelevantSources = [$(mkRelFile __FILE__)]
       }
