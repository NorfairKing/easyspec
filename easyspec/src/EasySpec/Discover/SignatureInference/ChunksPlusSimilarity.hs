module EasySpec.Discover.SignatureInference.ChunksPlusSimilarity where

import Import

import EasySpec.Discover.SignatureInference.ChunksPlusUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.Types

inferChunksPlusSimilarityName :: Int -> SignatureInferenceStrategy
inferChunksPlusSimilarityName i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus-similarity-name-" ++ show i
    , inferSignature = inferChunksPlusFrom $ diffChoice i simDiffName
    }

inferChunksPlusSimilarityType :: Int -> SignatureInferenceStrategy
inferChunksPlusSimilarityType i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus-similarity-type-" ++ show i
    , inferSignature = inferChunksPlusFrom $ diffChoice i simDiffType
    }

inferChunksPlusSimilaritySymbols :: Int -> SignatureInferenceStrategy
inferChunksPlusSimilaritySymbols i =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus-similarity-symbols-" ++ show i
    , inferSignature = inferChunksPlusFrom $ diffChoice i diffIdImpl
    }
