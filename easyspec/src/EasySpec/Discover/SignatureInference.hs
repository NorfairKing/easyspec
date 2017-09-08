module EasySpec.Discover.SignatureInference
    ( defaultInferenceStrategy
    , inferenceStrategies
    , inferenceStrategiesToEvaluate
    , interestingStrategies
    , evenMoreInferenceStrategies
    , inferFullBackground
    ) where

import Import

import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.ChunksPlus
import EasySpec.Discover.SignatureInference.ChunksPlusSimilarity
import EasySpec.Discover.SignatureInference.ChunksReachability
import EasySpec.Discover.SignatureInference.ChunksSimilarityName
import EasySpec.Discover.SignatureInference.ChunksSimilaritySymbols
import EasySpec.Discover.SignatureInference.ChunksSimilarityType
import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.IterativeChunks
import EasySpec.Discover.SignatureInference.SyntacticSimilarityEditDistanceName
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.TypeReachability
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

defaultInferenceStrategy :: SignatureInferenceStrategy
defaultInferenceStrategy = inferChunks

inferenceStrategies :: [SignatureInferenceStrategy]
inferenceStrategies =
    basicInferenceStrategies ++
    [ inferSyntacticSimilarityName 5 `unionInferAlg`
      inferSyntacticSimilaritySymbols 5
    , inferSyntacticSimilarityName 5 `unionInferAlg`
      inferSyntacticSimilarityType 5
    , inferSyntacticSimilaritySymbols 5 `unionInferAlg`
      inferSyntacticSimilarityType 5
    ]

inferenceStrategiesToEvaluate :: [SignatureInferenceStrategy]
inferenceStrategiesToEvaluate =
    [ inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName 5
    , inferSyntacticSimilaritySymbols 5
    , inferSyntacticSimilarityType 5
    , inferTypeReachability 7
    , inferChunks
    , inferChunksPlus
    , inferChunksSimilarityName 5
    , inferChunksSimilaritySymbols 5
    , inferChunksSimilarityType 5
    , inferChunksTypeReachability 7
    , inferChunksPlusSimilarityName 5
    , inferChunksPlusSimilaritySymbols 5
    , inferChunksPlusSimilarityType 5
    , inferChunksPlusTypeReachability 7
    , inferIterativeChunks 4 2
    , inferChunksPlusReachabilityName 7 5
    , inferChunksPlusReachabilitySymbols 7 5
    , inferChunksPlusReachabilityType 7 5
    ]

interestingStrategies :: [SignatureInferenceStrategy]
interestingStrategies =
    [inferEmptyBackground, inferFullBackground, inferChunks, inferChunksPlus]

basicInferenceStrategies :: [SignatureInferenceStrategy]
basicInferenceStrategies =
    nubBy ((==) `on` sigInfStratName) $
    defaultInferenceStrategy :
    [ inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName 5
    , inferSyntacticSimilarityEditDistanceName 5
    , inferSyntacticSimilaritySymbols 5
    , inferSyntacticSimilarityType 5
    , inferTypeReachability 7
    , inferChunks
    , inferChunksPlus
    , inferChunksSimilarityName 5
    , inferChunksSimilaritySymbols 5
    , inferChunksSimilarityType 5
    , inferChunksTypeReachability 7
    , inferChunksPlusSimilarityName 5
    , inferChunksPlusSimilaritySymbols 5
    , inferChunksPlusSimilarityType 5
    , inferChunksPlusTypeReachability 7
    , inferIterativeChunks 4 2
    , inferChunksPlusReachabilityName 7 5
    , inferChunksPlusReachabilitySymbols 7 5
    , inferChunksPlusReachabilityType 7 5
    ]

evenMoreInferenceStrategies :: [SignatureInferenceStrategy]
evenMoreInferenceStrategies = basicInferenceStrategies ++ unions
  where
    unions =
        map (uncurry unionInferAlg) $
        unorderedCombinations basicInferenceStrategies
