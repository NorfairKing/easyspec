module EasySpec.Discover.SignatureInference.ChunksSimilarityType where

import Import

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.Types

inferChunksSimilarityType :: Int -> SignatureInferenceStrategy
inferChunksSimilarityType i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-similarity-type-" ++ show i
    , inferSignature = inferChunksFrom $ diffChoice i simDiffType
    }
