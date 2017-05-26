{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.CorrelatingPoints
    ( plotsRulesForPointsPlotWithEvaluatorsPerExample
    , plotsRulesForPointsPlotWithEvaluators
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

plotsRulesForPointsPlotWithEvaluatorsPerExample ::
       ES.InputSpec -> Evaluator -> Evaluator -> Rules (Path Abs File)
plotsRulesForPointsPlotWithEvaluatorsPerExample is e1 e2 = do
    plotF <- pointsPlotForEvaluatorsPerExample is e1 e2
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        dataF <- dataFileForExample is
        needP [dataF]
        scriptF <- pointsPlotAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName e1
            , evaluatorName e2
            , toFilePath $ ES.inputSpecFile is
            , prettyIndication $ evaluatorIndication e1
            , prettyIndication $ evaluatorIndication e2
            ]
    pure plotF

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
            , "Global"
            , prettyIndication $ evaluatorIndication e1
            , prettyIndication $ evaluatorIndication e2
            ]
    pure plotF
