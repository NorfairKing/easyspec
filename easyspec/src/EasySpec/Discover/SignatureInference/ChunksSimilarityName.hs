module EasySpec.Discover.SignatureInference.ChunksSimilarityName where

import Import

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.Types

inferChunksSimilarityName :: Int -> SignatureInferenceStrategy
inferChunksSimilarityName i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-similarity-name-" ++ show i
    , inferSignature = inferChunksFrom $ diffChoice i simDiffName
    }
