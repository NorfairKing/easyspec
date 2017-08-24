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
            ]
    onDemandPlotRule ~> needP files
    pure onDemandPlotRule
