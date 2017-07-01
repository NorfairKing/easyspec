{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.CorrelatingPoints
    ( correlatingPointsPlotter
    , plotsRulesForPointsPlotWithEvaluatorsPerExample
    , plotsRulesForPointsPlotsWithGroupsOfExamples
    , plotsRulesForPointsPlotsWithGroupsOfExamplesPerStrategy
    , plotsRulesForPointsPlotWithEvaluators
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.R

correlatingPointsPlotter :: Plotter
correlatingPointsPlotter =
    "correlating-points"
    { plotterRulesOrderedDistinct2Evaluator =Just
          plotsRulesForPointsPlotWithEvaluators
    , plotterRulesGroupOrderedDistinct2Evaluator =
          Just plotsRulesForPointsPlotsWithGroupsOfExamples
    , plotterRulesGroupStrategyOrderedDistinct2Evaluator =
          Just plotsRulesForPointsPlotsWithGroupsOfExamplesPerStrategy
    , plotterRulesGroupExampleOrderedDistinct2Evaluator =
          Just plotsRulesForPointsPlotWithEvaluatorsPerExample
    }

plotsRulesForPointsPlotWithEvaluatorsPerExample ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> Example
    -> Evaluator
    -> Evaluator
    -> Rules ()
plotsRulesForPointsPlotWithEvaluatorsPerExample plotF dataF groupName is e1 e2 =
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
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

plotsRulesForPointsPlotsWithGroupsOfExamples ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> Evaluator
    -> Evaluator
    -> Rules ()
plotsRulesForPointsPlotsWithGroupsOfExamples plotF dataF groupName e1 e2 =
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        dataF <- dataFileForExampleGroup groupName
        needP [dataF]
        scriptF <- pointsPlotAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName e1
            , evaluatorName e2
            , "Per Group: " <> groupName
            , prettyIndication $ evaluatorIndication e1
            , prettyIndication $ evaluatorIndication e2
            ]

plotsRulesForPointsPlotsWithGroupsOfExamplesPerStrategy ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> SignatureInferenceStrategy
    -> Evaluator
    -> Evaluator
    -> Rules ()
plotsRulesForPointsPlotsWithGroupsOfExamplesPerStrategy plotF dataF groupName s e1 e2 =
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        dataF <- dataFileForExampleGroupAndStrategy groupName s
        needP [dataF]
        scriptF <- pointsPlotAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName e1
            , evaluatorName e2
            , "Per Group: " <> groupName <> " with strategy: " <>
              ES.sigInfStratName s
            , prettyIndication $ evaluatorIndication e1
            , prettyIndication $ evaluatorIndication e2
            ]

plotsRulesForPointsPlotWithEvaluators ::
       Path Abs File -> Path Abs File -> Evaluator -> Evaluator -> Rules ()
plotsRulesForPointsPlotWithEvaluators plotF dataF e1 e2 =
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

pointsPlotAnalysisScript :: MonadIO m => m (Path Abs File)
pointsPlotAnalysisScript = scriptFile "points.r"

