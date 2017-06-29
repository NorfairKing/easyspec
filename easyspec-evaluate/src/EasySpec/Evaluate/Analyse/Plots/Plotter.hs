{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Plots.Plotter
    ( Plotter(..)
    , plotter
    , plotRulesForPlotter
    ) where

import Import

import System.FilePath (dropExtensions)

import Development.Shake

-- Group of examples
--  \- Exmaple
--    \- Name
--
-- Strategy
-- Evaluator
import Import

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

plotRulesForPlotter :: Plotter -> Rules String
plotRulesForPlotter Plotter {..} = do
    plotterRule ~> pure ()
    pure plotterRule

data Plotter = Plotter
    { plotterRule :: String
    , plotterRulesEvaluatorGroup :: GroupName -> Rules ()
    , plotterRulesEvaluatorGroupExample :: GroupName -> Example -> Rules ()
    , plotterRulesEvaluatorGroupExampleName :: GroupName -> Example -> ExampleFunction -> Rules ()
    , plotterRulesEvaluatorGroupExampleNameEvaluator :: GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ()
    , plotterRulesEvaluatorGroupExampleNameOrderedUnequal2Evaluator :: GroupName -> Example -> ExampleFunction -> Evaluator -> Evaluator -> Rules ()
    }

plotter :: String -> Plotter
plotter name =
    Plotter
    { plotterRule = name
    , plotterRulesEvaluatorGroup = \_ -> pure ()
    , plotterRulesEvaluatorGroupExample = \_ _ -> pure ()
    , plotterRulesEvaluatorGroupExampleName = \_ _ _ -> pure ()
    , plotterRulesEvaluatorGroupExampleNameEvaluator = \_ _ _ _ -> pure ()
    , plotterRulesEvaluatorGroupExampleNameOrderedUnequal2Evaluator =
          \_ _ _ _ _ -> pure ()
    }

plotterEvaluatorGroupExampleNameOrderedUnequal2EvaluatorPlot ::
       MonadIO m => Plotter -> m (Path Abs File)
plotterEvaluatorGroupExampleNameOrderedUnequal2EvaluatorPlot p =
    plotterPlotFileWithComponents p
        [plotterRule p, "per-group-example-name-ordered-distict-evaluators"]
        []

plotterPlotFileWithComponents ::
       MonadIO m => Plotter -> [String] -> [String] -> m (Path Abs File)
plotterPlotFileWithComponents Plotter {..} dirStrs comps = do
    pd <- plotsDir
    let dirStr = intercalate "/" dirStrs
    let fileStr = dropExtensions $ intercalate "-" $ "plot" : comps
    liftIO $ resolveFile pd $ dirStr ++ "/" ++ fileStr ++ ".png"
