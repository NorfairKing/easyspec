{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.RelativeLines
    ( plotsRulesForLinesPlotWithEvaluator
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

plotsRulesForLinesPlotWithEvaluator :: Evaluator -> Rules (Path Abs File)
plotsRulesForLinesPlotWithEvaluator evaluator = do
    plotF <- linesPlotForEvaluator evaluator
    plotF $%> do
        dependOnEvaluator evaluator
        dataF <- allDataFile
        needP [dataF]
        scriptF <- linesPlotAnalysisScript
        rscript
            scriptF
            [toFilePath dataF, toFilePath plotF, evaluatorName evaluator]
    pure plotF
