{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.OnDemand
    ( onDemandPlotRule
    , onDemandPlotRules
    ) where

import Import

import Development.Shake

import EasySpec.Discover.SignatureInference.FullBackground
import EasySpec.Discover.SignatureInference.SyntacticSimilarityName
import EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Evaluate.Evaluator

onDemandPlotRule :: String
onDemandPlotRule = "on-demand"

onDemandPlotRules :: Rules String
onDemandPlotRules = do
    rules <-
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
            ]
    onDemandPlotRule ~> need rules
    pure onDemandPlotRule
