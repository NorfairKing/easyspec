module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( boxPlotterPerGroupExampleEvaluator
    , boxPlotterPerEvaluatorStrategies
    , boxPlotterPerEvaluator
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Types

boxPlotterPerGroupExampleEvaluator ::
       EvaluatedCartPlotter (GroupAndExample, Evaluator)
boxPlotterPerGroupExampleEvaluator =
    CartPlotter
    { cartPlotterName = "evaluator-box"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_average.r"
    }

boxPlotterPerEvaluatorStrategies ::
       EvaluatedCartPlotter ( GroupName
                            , Evaluator
                            , OrderedDistinct SignatureInferenceStrategy)
boxPlotterPerEvaluatorStrategies =
    CartPlotter
    { cartPlotterName = "evaluator-box"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_strategies.r"
    }

boxPlotterPerEvaluator :: EvaluatedCartPlotter (GroupName, Evaluator)
boxPlotterPerEvaluator =
    CartPlotter
    { cartPlotterName = "evaluator-box"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_average_global.r"
    }
