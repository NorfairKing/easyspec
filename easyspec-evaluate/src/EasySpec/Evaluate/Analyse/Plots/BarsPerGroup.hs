{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
    ( barsPerGroupEvaluatorsStrategiesPlotter
    , barsPerGroupEvaluatorsStrategyPlotter
    , barsPerGroupEvaluatorsPlotter
    ) where

import Import

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.R

barsPerGroupEvaluatorsStrategiesPlotter ::
       EvaluatedCartPlotter ( GroupName
                            , IndepDepPairEvaluator
                            , OrderedDistinct SignatureInferenceStrategy)
barsPerGroupEvaluatorsStrategiesPlotter =
    CartPlotter
    { cartPlotterName = "evaluator-bars"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $ do
              needRLibs ["dplyr", "ggplot2"]
              scriptFile "evaluator_bars_per_group_strategies.r"
    }

barsPerGroupEvaluatorsStrategyPlotter ::
       EvaluatedCartPlotter ( GroupName
                            , SignatureInferenceStrategy
                            , IndepDepPairEvaluator)
barsPerGroupEvaluatorsStrategyPlotter =
    CartPlotter
    { cartPlotterName = "evaluator-bars"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $ do
              needRLibs ["dplyr", "ggplot2"]
              scriptFile "evaluator_bars_per_group_strategy.r"
    }

barsPerGroupEvaluatorsPlotter ::
       EvaluatedCartPlotter (GroupName, IndepDepPairEvaluator)
barsPerGroupEvaluatorsPlotter =
    CartPlotter
    { cartPlotterName = "evaluators-bars"
    , cartPlotterFunc =
          standardisedEvaluatedPlotruleFor $
          scriptFile "evaluators_bars_per_group.r"
    }
