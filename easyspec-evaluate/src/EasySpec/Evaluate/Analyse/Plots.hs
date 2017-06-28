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

import EasySpec.Evaluate.Evaluate.Evaluator

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.CorrelatingPoints
import EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
import EasySpec.Evaluate.Analyse.Plots.DistributionOccurrencesInSameEquation
import EasySpec.Evaluate.Analyse.Plots.DistributionSizeOfProperty
import EasySpec.Evaluate.Analyse.Plots.RelativeLines
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
import EasySpec.Evaluate.Analyse.Utils

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    allDataPlotsFs <- plotsRulesForAllData
    groupPlotsFs <- concat <$> mapM (uncurry plotsRulesForExampleGroup) exampleGroups
    plotsFs <- concat <$> mapM plotsRulesForExample examples
    plotsRule ~> needP (allDataPlotsFs ++ groupPlotsFs ++ plotsFs)

plotsRulesForAllData :: Rules [Path Abs File]
plotsRulesForAllData = do
    lfs <- mapM plotsRulesForLinesPlotWithEvaluator evaluators
    pfs <-
        mapM (uncurry plotsRulesForPointsPlotWithEvaluators) $
        unorderedCombinationsWithoutSelfCombinations evaluators
    bfs <- mapM perEvaluatorGlobalAverageBoxPlotFor evaluators
    dnrdfs <- plotsRulesDistributionNrDifferentFunctions
    oosfies <- plotsRulesDistributionDistributionOccurrencesInSameEquation
    dsofs <- plotsRulesDistributionDistributionSizeOfProperty
    pure $ lfs ++ pfs ++ bfs ++ dnrdfs ++ oosfies ++ dsofs

plotsRulesForExampleGroup :: String -> [ES.InputSpec] -> Rules [Path Abs File]
plotsRulesForExampleGroup groupName exs = do
    mapM
        (uncurry $ plotsRulesForPointsPlotsWithGroupsOfExamples groupName exs)
        (unorderedCombinationsWithoutSelfCombinations evaluators)

plotsRulesForExample :: ES.InputSpec -> Rules [Path Abs File]
plotsRulesForExample is = do
    names <- liftIO $ namesInSource is
    bars <- fmap concat $ forM names $ plotsRulesForExampleAndName is
    boxes <- forM evaluators $ perExampleAndEvaluatorAverageBoxPlotFor is
    points <-
        mapM (uncurry (plotsRulesForPointsPlotWithEvaluatorsPerExample is)) $
        unorderedCombinationsWithoutSelfCombinations evaluators
    pure $ bars ++ boxes ++ points

plotsRulesForExampleAndName ::
       ES.InputSpec -> ES.EasyQName -> Rules [Path Abs File]
plotsRulesForExampleAndName is name =
    forM evaluators $ perExampleNameAndEvaluatorBarPlotFor is name
