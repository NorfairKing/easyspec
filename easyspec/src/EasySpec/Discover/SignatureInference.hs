module EasySpec.Discover.SignatureInference
    ( inferenceStrategies
    , inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName
    ) where

import Import

import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.Types

inferenceStrategies :: [SignatureInferenceStrategy]
inferenceStrategies =
    [ inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName
    , inferSyntacticSimilarityEditDistanceName
    , inferSyntacticSimilaritySymbols
    , inferSyntacticSimilarityType
    ]
