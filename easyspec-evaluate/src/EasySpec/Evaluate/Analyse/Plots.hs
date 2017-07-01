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
import EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
import EasySpec.Evaluate.Analyse.Plots.DistributionOccurrencesInSameEquation
import EasySpec.Evaluate.Analyse.Plots.DistributionSizeOfProperty
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.RelativeLines
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Analyse.Utils

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    allDataPlotsFs <- plotsRulesForAllData
    plotsFs <- concat <$> mapM (uncurry plotsRulesForExample) groupsAndExamples
    correlatingPointsRule <- plotRulesForPlotter correlatingPointsPlotter
    barPlotsRule <- plotRulesForPlotter barPlotter
    plotsRule ~> do
        need [correlatingPointsRule, barPlotsRule]
        needP (allDataPlotsFs ++ plotsFs)

plotsRulesForAllData :: Rules [Path Abs File]
plotsRulesForAllData = do
    lfs <- mapM plotsRulesForLinesPlotWithEvaluator evaluators
    bfs <- mapM perEvaluatorGlobalAverageBoxPlotFor evaluators
    dnrdfs <- plotsRulesDistributionNrDifferentFunctions
    oosfies <- plotsRulesDistributionDistributionOccurrencesInSameEquation
    dsofs <- plotsRulesDistributionDistributionSizeOfProperty
    pure $ lfs ++ bfs ++ dnrdfs ++ oosfies ++ dsofs

plotsRulesForExample :: GroupName -> Example -> Rules [Path Abs File]
plotsRulesForExample groupName is = do
    names <- liftIO $ namesInSource is
    forM evaluators $ perExampleAndEvaluatorAverageBoxPlotFor groupName is
