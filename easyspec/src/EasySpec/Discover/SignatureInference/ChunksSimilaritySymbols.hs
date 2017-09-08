module EasySpec.Discover.SignatureInference.ChunksSimilaritySymbols where

import Import

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.Types

inferChunksSimilaritySymbols :: Int -> SignatureInferenceStrategy
inferChunksSimilaritySymbols i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-similarity-symbols-" ++ show i
    , inferSignature = inferChunksFrom $ diffChoice i diffIdImpl
    }
