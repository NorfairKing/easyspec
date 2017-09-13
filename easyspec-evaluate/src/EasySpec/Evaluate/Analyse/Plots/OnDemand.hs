{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.OnDemand
    ( onDemandPlotRule
    , onDemandPlotRules
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.ChunksPlus
import EasySpec.Discover.SignatureInference.ChunksReachability
import EasySpec.Discover.SignatureInference.ChunksSimilarityName
import EasySpec.Discover.SignatureInference.ChunksSimilaritySymbols
import EasySpec.Discover.SignatureInference.ChunksPlusSimilarity
import EasySpec.Discover.SignatureInference.ChunksSimilarityType
import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.TypeReachability
import EasySpec.Evaluate.Analyse.Plots.BarsPerGroup

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Evaluate.Evaluator

onDemandPlotRule :: String
onDemandPlotRule = "on-demand"

onDemandPlotRules :: Rules String
onDemandPlotRules = do
    files <-
        sequence
            [ onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [ inferFullBackground
                    , inferSyntacticSimilarityName 5
                    , inferSyntacticSimilaritySymbols 5
                    , inferSyntacticSimilarityType 5
                    ])
            , onDemandEvaluatedCartRule
                  barsPerGroupEvaluatorsStrategiesPlotterOnDemand
                  ( runtimeGroup
                  , IndepDepPairEvaluator
                        (Pair scopeSizeEvaluator runtimeEvaluator)
                  , [ inferFullBackground
                    , inferSyntacticSimilarityName 5
                    , inferSyntacticSimilaritySymbols 5
                    , inferSyntacticSimilarityType 5
                    ])
            , onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [ inferFullBackground
                    , inferSyntacticSimilarityName 5
                    , inferSyntacticSimilaritySymbols 5
                    , inferSyntacticSimilarityType 5
                    , inferTypeReachability 7
                    ])
            , onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [inferFullBackground, inferChunks, inferChunksPlus])
            , onDemandEvaluatedCartRule
                  barsPerGroupEvaluatorsStrategiesPlotterOnDemand
                  ( runtimeGroup
                  , IndepDepPairEvaluator
                        (Pair scopeSizeEvaluator runtimeEvaluator)
                  , [inferFullBackground, inferChunks, inferChunksPlus])
            , onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [ inferFullBackground
                    , inferChunksSimilarityName 5
                    , inferChunksSimilaritySymbols 5
                    , inferChunksSimilarityType 5
                    , inferChunksTypeReachability 7
                    ])
            , onDemandEvaluatedCartRule
                  barsPerGroupEvaluatorsStrategiesPlotterOnDemand
                  ( runtimeGroup
                  , IndepDepPairEvaluator
                        (Pair scopeSizeEvaluator runtimeEvaluator)
                  , [ inferFullBackground
                    , inferChunksSimilarityName 5
                    , inferChunksSimilaritySymbols 5
                    , inferChunksSimilarityType 5
                    , inferChunksTypeReachability 7
                    ])
            , onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [ inferFullBackground
                    , inferChunksPlusSimilarityName 5
                    , inferChunksPlusSimilaritySymbols 5
                    , inferChunksPlusSimilarityType 5
                    , inferChunksPlusTypeReachability 7
                    ])
            , onDemandEvaluatedCartRule
                  barsPerGroupEvaluatorsStrategiesPlotterOnDemand
                  ( runtimeGroup
                  , IndepDepPairEvaluator
                        (Pair scopeSizeEvaluator runtimeEvaluator)
                  , [ inferFullBackground
                    , inferChunksPlusSimilarityName 5
                    , inferChunksPlusSimilaritySymbols 5
                    , inferChunksPlusSimilarityType 5
                    , inferChunksPlusTypeReachability 7
                    ])
            , onDemandEvaluatedCartRule
                  boxPlotterPerGroupEvaluatorOnDemand
                  ( evaluationGroup
                  , relevantEquationsEvaluator
                  , [ inferFullBackground
                    , inferChunksPlusReachabilityName 7 5
                    , inferChunksPlusReachabilitySymbols 7 5
                    , inferChunksPlusReachabilityType 7 5
                    ])
            , onDemandEvaluatedCartRule
                  barsPerGroupEvaluatorsStrategiesPlotterOnDemand
                  ( runtimeGroup
                  , IndepDepPairEvaluator
                        (Pair scopeSizeEvaluator runtimeEvaluator)
                  , [ inferFullBackground
                    , inferChunksPlusReachabilityName 7 5
                    , inferChunksPlusReachabilitySymbols 7 5
                    , inferChunksPlusReachabilityType 7 5
                    ])
            ]
    onDemandPlotRule ~> needP files
    pure onDemandPlotRule
