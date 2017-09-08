module EasySpec.Discover.SignatureInference.ChunksReachability where

import Import

import EasySpec.Discover.SignatureInference.ChunksPlusUtils
import EasySpec.Discover.SignatureInference.ShrinkThenDrill
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.TypeReachability
import EasySpec.Discover.Types

inferChunksPlusReachabilityName :: Int -> Int -> SignatureInferenceStrategy
inferChunksPlusReachabilityName j i =
    SignatureInferenceStrategy
    { sigInfStratName =
          intercalate "-" ["chunks-plus-reachability-name", show i, show j]
    , inferSignature = inferChunksPlusReachability j $ diffChoice i simDiffName
    }

inferChunksPlusReachabilitySymbols :: Int -> Int -> SignatureInferenceStrategy
inferChunksPlusReachabilitySymbols j i =
    SignatureInferenceStrategy
    { sigInfStratName =
          intercalate "-" ["chunks-plus-reachability-symbols", show i, show j]
    , inferSignature = inferChunksPlusReachability j $ diffChoice i simDiffType
    }

inferChunksPlusReachabilityType :: Int -> Int -> SignatureInferenceStrategy
inferChunksPlusReachabilityType j i =
    SignatureInferenceStrategy
    { sigInfStratName =
          intercalate "-" ["chunks-plus-reachability-type", show i, show j]
    , inferSignature = inferChunksPlusReachability j $ diffChoice i diffIdImpl
    }

inferChunksPlusReachability ::
       Int
    -> ([EasyId] -> [EasyId] -> [EasyId])
    -> [EasyId]
    -> [EasyId]
    -> InferredSignature
inferChunksPlusReachability j secondShrink =
    inferShrinkThenDrill shrink drillChunksPlus
  where
    shrink = composeShrinks firstShrink secondShrink
    firstShrink = depthNReachableViaComposition j
