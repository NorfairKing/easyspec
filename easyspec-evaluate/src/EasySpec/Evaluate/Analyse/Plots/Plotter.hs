{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Plots.Plotter
    ( Plotter(..)
    , plotter
    , plotRulesForPlotter
    ) where

import Import hiding (group)

import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath (dropExtensions)

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

-- Group of examples
--  \- Exmaple
--    \- Name
--
-- Strategy
-- Evaluator
plotRulesForPlotter :: Plotter -> Rules String
plotRulesForPlotter p@Plotter {..} = do
    perGroupPlots <-
        rule plotterRulesEvaluatorGroup $ \func ->
            forM groups $ \group -> do
                dataF <- undefined
                plotF <- plotterEvaluatorGroupPlot p group
                func plotF dataF group
                pure plotF
    perGroupExamplePlots <-
        rule plotterRulesEvaluatorGroupExample $ \func ->
            forM groupsAndExamples $ \(group, example) -> do
                dataF <- undefined
                plotF <- plotterEvaluatorGroupExamplePlot p group example
                func plotF dataF group example
                pure plotF
    perGroupExampleNamePlots <-
        rule plotterRulesEvaluatorGroupExampleName $ \func -> do
            trips <- groupsExamplesAndNames
            forM trips $ \(group, example, name) -> do
                dataF <- undefined
                plotF <-
                    plotterEvaluatorGroupExampleNamePlot p group example name
                func plotF dataF group example name
                pure plotF
    perGroupExampleNameEvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleNameEvaluator $ \func -> do
            quads <- groupExamplesNamesAndEvaluators
            forM quads $ \(group, example, name, evaluator) -> do
                dataF <- undefined
                plotF <-
                    plotterEvaluatorGroupExampleNameEvaluatorPlot
                        p
                        group
                        example
                        name
                        evaluator
                func plotF dataF group example name evaluator
                pure plotF
    perGroupExampleNameOrderedUnequal2EvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleNameOrderedUnequal2Evaluator $ \func -> do
            trips <- groupsExamplesAndNames
            fmap concat $
                forM trips $ \(group, example, name) ->
                    forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                        dataF <- undefined
                        plotF <-
                            plotterEvaluatorGroupExampleNameOrderedUnequal2EvaluatorPlot
                                p
                                group
                                example
                                name
                                e1
                                e2
                        func plotF dataF group example name e1 e2
                        pure plotF
    let plots =
            perGroupPlots ++
            perGroupExamplePlots ++
            perGroupExampleNamePlots ++
            perGroupExampleNameEvaluatorPlots ++
            perGroupExampleNameOrderedUnequal2EvaluatorPlots
    plotterRule ~> needP plots
    pure plotterRule
  where
    rule :: Applicative f => Maybe a -> (a -> f [b]) -> f [b]
    rule Nothing _ = pure []
    rule (Just a) func = func a

data Plotter = Plotter
    { plotterRule :: String
    , plotterRulesEvaluatorGroup :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Rules ())
    , plotterRulesEvaluatorGroupExample :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Rules ())
    , plotterRulesEvaluatorGroupExampleName :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Rules ())
    , plotterRulesEvaluatorGroupExampleNameEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ())
    , plotterRulesEvaluatorGroupExampleNameOrderedUnequal2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Evaluator -> Rules ())
    }

plotter :: String -> Plotter
plotter name =
    Plotter
    { plotterRule = name
    , plotterRulesEvaluatorGroup = Nothing
    , plotterRulesEvaluatorGroupExample = Nothing
    , plotterRulesEvaluatorGroupExampleName = Nothing
    , plotterRulesEvaluatorGroupExampleNameEvaluator = Nothing
    , plotterRulesEvaluatorGroupExampleNameOrderedUnequal2Evaluator = Nothing
    }

plotterEvaluatorGroupPlot ::
       MonadIO m => Plotter -> GroupName -> m (Path Abs File)
plotterEvaluatorGroupPlot p g = plotterPlotFile p ["per-group"] [g]

plotterEvaluatorGroupExamplePlot ::
       MonadIO m => Plotter -> GroupName -> Example -> m (Path Abs File)
plotterEvaluatorGroupExamplePlot p g e =
    plotterPlotFile p ["per-group-example"] [g, exampleModule e]

plotterEvaluatorGroupExampleNamePlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> ExampleFunction
    -> m (Path Abs File)
plotterEvaluatorGroupExampleNamePlot p g e n =
    plotterPlotFile
        p
        ["per-group-example-name"]
        [g, exampleModule e, prettyPrint n]

plotterEvaluatorGroupExampleNameEvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> ExampleFunction
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupExampleNameEvaluatorPlot p g e n e1 =
    plotterPlotFile
        p
        ["per-group-example-name"]
        [g, exampleModule e, prettyPrint n, evaluatorName e1]

plotterEvaluatorGroupExampleNameOrderedUnequal2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> ExampleFunction
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupExampleNameOrderedUnequal2EvaluatorPlot p g e n e1 e2 =
    plotterPlotFile
        p
        ["per-group-example-name-ordered-distict-evaluators"]
        [g, exampleModule e, prettyPrint n, evaluatorName e1, evaluatorName e2]

exampleModule :: Example -> String
exampleModule = map go . dropExtensions . toFilePath . ES.inputSpecFile
  where
    go :: Char -> Char
    go '/' = '.'
    go c = c

plotterPlotFile ::
       MonadIO m => Plotter -> [String] -> [String] -> m (Path Abs File)
plotterPlotFile p dircomps =
    plotterRawPlotFileWithComponents p (plotterRule p : dircomps)

plotterRawPlotFileWithComponents ::
       MonadIO m => Plotter -> [String] -> [String] -> m (Path Abs File)
plotterRawPlotFileWithComponents Plotter {..} dirStrs comps = do
    pd <- plotsDir
    let dirStr = intercalate "/" dirStrs
    let fileStr = dropExtensions $ intercalate "-" $ "plot" : comps
    liftIO $ resolveFile pd $ dirStr ++ "/" ++ fileStr ++ ".png"
