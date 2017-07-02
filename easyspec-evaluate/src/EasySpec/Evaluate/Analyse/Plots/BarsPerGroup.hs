{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
    ( barsPerGroupEvaluatorsPlotter
    ) where

import Import

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter

barsPerGroupEvaluatorsPlotter ::
       EvaluatedCartPlotter (GroupName, UnorderedDistinct Evaluator)
barsPerGroupEvaluatorsPlotter =
    CartPlotter
    { cartPlotterName = "evaluators-bars"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "evaluators_bars_per_group.r"
    }
