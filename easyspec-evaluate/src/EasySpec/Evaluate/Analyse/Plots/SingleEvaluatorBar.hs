module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
    ( barPlotter
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter

barPlotter :: EvaluatedCartPlotter (GroupAndExampleAndName, Evaluator)
barPlotter =
    CartPlotter
    { cartPlotterName = "evaluator-bar"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $ scriptFile "single_evaluator_bar.r"
    }
