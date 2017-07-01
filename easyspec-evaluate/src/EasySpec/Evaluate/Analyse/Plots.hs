{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots
    ( plotsRule
    , plotsRules
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.CorrelatingPoints
import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
import EasySpec.Evaluate.Analyse.Plots.DistributionOccurrencesInSameEquation
import EasySpec.Evaluate.Analyse.Plots.DistributionSizeOfProperty
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Analyse.Utils

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    allDataPlotsFs <- plotsRulesForAllData
    rules <-
        mapM
            plotRulesForPlotter
            [ correlatingPointsPlotter
            , barPlotter
            , boxPlotter
            , dfrgPlotter dfrgSizeOfProperty
            ]
    plotsRule ~> do
        need rules
        needP allDataPlotsFs

plotsRulesForAllData :: Rules [Path Abs File]
plotsRulesForAllData = do
    dnrdfs <- plotsRulesDistributionNrDifferentFunctions
    oosfies <- plotsRulesDistributionDistributionOccurrencesInSameEquation
    dsofs <- plotsRulesDistributionDistributionSizeOfProperty
    pure $ dnrdfs ++ oosfies ++ dsofs
