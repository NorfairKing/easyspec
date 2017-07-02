{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
    ( barsPerGroupEvaluatorsPlotter
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.R

barsPerGroupEvaluatorsPlotter :: Plotter
barsPerGroupEvaluatorsPlotter =
    "evaluator-bars-per-group"
    { plotterRulesGroupUnorderedDistinct2Evaluator =
          Just perGroupEvaluatorsBarsPlotFor
    }

perGroupEvaluatorsBarsPlotFor ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> Evaluator
    -> Evaluator
    -> Rules ()
perGroupEvaluatorsBarsPlotFor plotF dataF g e1 e2 =
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        needP [dataF]
        scriptF <- scriptFile "evaluators_bars_per_group.r"
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , g
            , evaluatorName e1
            , evaluatorName e2
            ]
