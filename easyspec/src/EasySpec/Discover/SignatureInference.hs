module EasySpec.Discover.SignatureInference
    ( defaultInferenceStrategy
    , inferenceStrategies
    , inferenceStrategiesToEvaluate
    , evenMoreInferenceStrategies
    , inferFullBackground
    ) where

import Import

import EasySpec.Discover.SignatureInference.EmptyBackground
import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.FullBreakthrough
import EasySpec.Discover.SignatureInference.SyntacticSimilarityEditDistanceName
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.TypeReachability
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

defaultInferenceStrategy :: SignatureInferenceStrategy
defaultInferenceStrategy = inferFullBreakthrough 1

inferenceStrategies :: [SignatureInferenceStrategy]
inferenceStrategies =
    basicInferenceStrategies ++
    [ inferSyntacticSimilarityName `unionInferAlg`
      inferSyntacticSimilaritySymbols
    , inferSyntacticSimilarityName `unionInferAlg` inferSyntacticSimilarityType
    , inferSyntacticSimilaritySymbols `unionInferAlg`
      inferSyntacticSimilarityType
    ]

inferenceStrategiesToEvaluate :: [SignatureInferenceStrategy]
inferenceStrategiesToEvaluate =
    [ inferEmptyBackground
    , inferFullBackground
    , inferFullBreakthrough 1
    , inferSyntacticSimilarityType
    , inferTypeReachability 7
    ]

basicInferenceStrategies :: [SignatureInferenceStrategy]
basicInferenceStrategies =
    nubBy ((==) `on` sigInfStratName) $
    defaultInferenceStrategy :
    [ inferEmptyBackground
    , inferFullBackground
    , inferSyntacticSimilarityName
    , inferFullBreakthrough 1
    , inferSyntacticSimilarityEditDistanceName
    , inferSyntacticSimilaritySymbols
    , inferSyntacticSimilarityType
    , inferTypeReachability 7
    ]

evenMoreInferenceStrategies :: [SignatureInferenceStrategy]
evenMoreInferenceStrategies = basicInferenceStrategies ++ unions
  where
    unions =
        map (uncurry unionInferAlg) $
        unorderedCombinations basicInferenceStrategies
