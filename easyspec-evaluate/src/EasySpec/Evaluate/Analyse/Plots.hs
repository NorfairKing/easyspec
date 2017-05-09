{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots
    ( plotsRule
    , plotsRules
    ) where

import Import

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Evaluate.Evaluator

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Plots.RelativeLines
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    es <- examples
    allDataPlotsFs <- plotsRulesForAllData
    plotsFs <- concat <$> mapM plotsRulesForExample es
    plotsRule ~> needP (allDataPlotsFs ++ plotsFs)

plotsRulesForAllData :: Rules [Path Abs File]
plotsRulesForAllData = mapM plotsRulesForLinesPlotWithEvaluator evaluators

plotsRulesForExample :: ES.InputSpec -> Rules [Path Abs File]
plotsRulesForExample is = do
    names <- namesInSource is
    bars <- fmap concat $ forM names $ plotsRulesForExampleAndName is
    boxes <- forM evaluators $ perExampleAndEvaluatorAverageBoxPlotFor is
    pure $ bars ++ boxes

plotsRulesForExampleAndName ::
       ES.InputSpec -> ES.EasyName -> Rules [Path Abs File]
plotsRulesForExampleAndName is name =
    forM evaluators $ perExampleNameAndEvaluatorBarPlotFor is name
