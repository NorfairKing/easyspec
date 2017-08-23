module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( boxPlotterPerGroupExampleEvaluator
    , boxPlotterPerGroupEvaluatorOnDemand
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

boxPlotterPerGroupEvaluatorOnDemand ::
       EvaluatedCartPlotter (GroupName, Evaluator, [SignatureInferenceStrategy])
boxPlotterPerGroupEvaluatorOnDemand =
    CartPlotter
    { cartPlotterName = "evaluator-box-on-demand"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "single_evaluator_boxplot_on_demand.r"
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
