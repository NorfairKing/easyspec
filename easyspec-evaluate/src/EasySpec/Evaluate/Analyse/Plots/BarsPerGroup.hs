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

barsPerGroupEvaluatorsPlotter ::
       EvaluatedCartPlotter (GroupName, UnorderedDistinct Evaluator)
barsPerGroupEvaluatorsPlotter =
    CartPlotter
    { cartPlotterName = "evaluators-bars"
    , cartPlotterFunc = perGroupEvaluatorsBarsPlotFor
    }

perGroupEvaluatorsBarsPlotFor ::
       Path Abs File
    -> Action (Path Abs File)
    -> (GroupName, UnorderedDistinct Evaluator)
    -> Rules ()
perGroupEvaluatorsBarsPlotFor plotF getDataF (g, UnorderedDistinct e1 e2) =
    plotF $%> do
        dependOnEvaluator e1
        dependOnEvaluator e2
        dataF <- getDataF
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
