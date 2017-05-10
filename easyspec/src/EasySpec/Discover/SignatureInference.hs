module EasySpec.Discover.SignatureInference
    ( inferenceStrategies
    , inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName
    , evenMoreInferenceStrategies
    ) where

import Import

import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityEditDistanceName
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferenceStrategies :: [SignatureInferenceStrategy]
inferenceStrategies =
    basicInferenceStrategies ++
    [ inferSyntacticSimilarityName `unionInferAlg`
      inferSyntacticSimilaritySymbols
    , inferSyntacticSimilarityName `unionInferAlg` inferSyntacticSimilarityType
    , inferSyntacticSimilaritySymbols `unionInferAlg`
      inferSyntacticSimilarityType
    ]

basicInferenceStrategies :: [SignatureInferenceStrategy]
basicInferenceStrategies =
    [ inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName
    , inferSyntacticSimilarityEditDistanceName
    , inferSyntacticSimilaritySymbols
    , inferSyntacticSimilarityType
    ]

evenMoreInferenceStrategies :: [SignatureInferenceStrategy]
evenMoreInferenceStrategies = basicInferenceStrategies ++ unions
  where
    unions =
        map (uncurry unionInferAlg) $
        unorderedCombinations basicInferenceStrategies
