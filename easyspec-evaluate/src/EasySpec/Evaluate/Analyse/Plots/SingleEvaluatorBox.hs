module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( boxPlotterPerGroupExampleEvaluator
    , boxPlotterPerEvaluator
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter

boxPlotterPerGroupExampleEvaluator ::
       EvaluatedCartPlotter (GroupAndExample, Evaluator)
boxPlotterPerGroupExampleEvaluator =
    CartPlotter
    { cartPlotterName = "evaluator-box"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_average.r"
    }

boxPlotterPerEvaluator :: EvaluatedCartPlotter Evaluator
boxPlotterPerEvaluator =
    CartPlotter
    { cartPlotterName = "evaluator-box"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_average_global.r"
    }
