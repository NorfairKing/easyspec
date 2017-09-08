{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots
    ( plotsRule
    , plotsRules
    ) where

import Import

import Development.Shake

import EasySpec.Evaluate.Analyse.Plots.BarsPerGroup
import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
import EasySpec.Evaluate.Analyse.Plots.OnDemand
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    rules <-
        concat <$>
        sequence
            [ dfrgRules dfrgNrDifferentFunctions
            -- , dfrgRules dfrgSizeOfProperty
            -- , dfrgRules dfrgOccurrencesInAllEquations
            -- , dfrgRules dfrgOccurrencesInSameEquation
            ]
    rules' <-
        sequence
            [ evaluatedCartRule barsPerGroupEvaluatorsStrategiesPlotter
            , evaluatedCartRule barsPerGroupEvaluatorsStrategyPlotter
            , evaluatedCartRule boxPlotterPerEvaluatorStrategies
            , evaluatedCartRule boxPlotterPerEvaluator
            , evaluatedCartRule barsPerGroupEvaluatorsPlotter
            -- , evaluatedCartRule barPlotter
            -- , evaluatedCartRule boxPlotterPerGroupExampleEvaluator
            -- , evaluatedCartRule correlatingPointsPlotterAll
            -- , evaluatedCartRule correlatingPointsPlotterPerGroup
            -- , evaluatedCartRule correlatingPointsPlotterPerGroupExample
            -- , evaluatedCartRule correlatingPointsPlotterPerGroupStrategy
            ]
    odRule <- onDemandPlotRules
    plotsRule ~> need (odRule : rules ++ rules')
