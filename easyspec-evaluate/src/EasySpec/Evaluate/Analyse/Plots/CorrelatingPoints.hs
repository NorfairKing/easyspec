{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.CorrelatingPoints
    ( plotsRulesForPointsPlotWithEvaluators
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

plotsRulesForPointsPlotWithEvaluators ::
       Evaluator -> Evaluator -> Rules (Path Abs File)
plotsRulesForPointsPlotWithEvaluators e1 e2 = do
    plotF <- pointsPlotForEvaluators e1 e2
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        dataF <- allDataFile
        needP [dataF]
        scriptF <- pointsPlotAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName e1
            , evaluatorName e2
            ]
    pure plotF
