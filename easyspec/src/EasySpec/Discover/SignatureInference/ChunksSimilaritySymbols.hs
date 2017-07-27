
module EasySpec.Discover.SignatureInference.ChunksSimilaritySymbols where

import Import

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.Types

inferChunksSimilaritySymbols :: Int -> SignatureInferenceStrategy
inferChunksSimilaritySymbols i =
    (inferChunksFrom $ diffChoice i diffIdImpl)
    { sigInfStratName = "chunks-similarity-symbols-" ++ show i
    }
