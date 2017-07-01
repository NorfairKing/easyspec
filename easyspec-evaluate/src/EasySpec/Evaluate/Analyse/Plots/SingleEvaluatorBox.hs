{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( boxPlotter
    ) where

import Import

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

boxPlotter :: Plotter
boxPlotter =
    "evaluator-box"
    { plotterRulesGroupExampleEvaluator =
          Just perExampleAndEvaluatorAverageBoxPlotFor
    , plotterRulesEvaluator = Just perEvaluatorGlobalAverageBoxPlotFor
    }

perExampleAndEvaluatorAverageBoxPlotFor ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> Example
    -> Evaluator
    -> Rules ()
perExampleAndEvaluatorAverageBoxPlotFor plotF dataF groupName is evaluator =
    plotF $%> do
        dependOnEvaluator evaluator
        dataF <- dataFileForExample groupName is
        needP [dataF]
        scriptF <- singleEvaluatorAverageBoxAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , toFilePath $ ES.inputSpecBaseDir is
            , toFilePath $ ES.inputSpecFile is
            , evaluatorName evaluator
            , prettyIndication $ evaluatorIndication evaluator
            ]

perEvaluatorGlobalAverageBoxPlotFor ::
       Path Abs File -> Path Abs File -> Evaluator -> Rules ()
perEvaluatorGlobalAverageBoxPlotFor plotF dataF evaluator =
    plotF $%> do
        dependOnEvaluator evaluator
        dataF <- allDataFile
        needP [dataF]
        scriptF <- singleEvaluatorAverageBoxGlobalAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName evaluator
            , prettyIndication $ evaluatorIndication evaluator
            ]
