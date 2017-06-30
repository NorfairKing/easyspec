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

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Files
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
                dataF <- dataFileForExampleGroup group
                plotF <- plotterEvaluatorGroupPlot p group
                func plotF dataF group
                pure plotF
    perGroupExamplePlots <-
        rule plotterRulesEvaluatorGroupExample $ \func ->
            forM groupsAndExamples $ \(group, example) -> do
                dataF <- dataFileForExample group example
                plotF <- plotterEvaluatorGroupExamplePlot p group example
                func plotF dataF group example
                pure plotF
    perGroupExampleNamePlots <-
        rule plotterRulesEvaluatorGroupExampleName $ \func -> do
            trips <- groupsExamplesAndNames
            forM trips $ \(group, example, name) -> do
                dataF <- dataFileForExampleAndName group example name
                plotF <-
                    plotterEvaluatorGroupExampleNamePlot p group example name
                func plotF dataF group example name
                pure plotF
    perGroupExampleEvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleEvaluator $ \func -> do
            forM groupExampleEvaluators $ \(group, example, evaluator) -> do
                dataF <-
                    evaluatedFileForGroupExampleEvaluator
                        group
                        example
                        evaluator
                plotF <-
                    plotterEvaluatorGroupExampleEvaluatorPlot
                        p
                        group
                        example
                        evaluator
                func plotF dataF group example evaluator
                pure plotF
    perGroupExampleOrderedDistinct2EvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleOrderedDistinct2Evaluator $ \func ->
            fmap concat $
            forM groupsAndExamples $ \(group, example) ->
                forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                    dataF <- evaluatedFileForGroupExample group example
                    plotF <-
                        plotterEvaluatorGroupExampleOrderedDistinct2EvaluatorPlot
                            p
                            group
                            example
                            e1
                            e2
                    func plotF dataF group example e1 e2
                    pure plotF
    perGroupExampleNameEvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleNameEvaluator $ \func -> do
            quads <- groupExamplesNamesAndEvaluators
            forM quads $ \(group, example, name, evaluator) -> do
                dataF <-
                    evaluatedFileForGroupExampleNameEvaluator
                        group
                        example
                        name
                        evaluator
                plotF <-
                    plotterEvaluatorGroupExampleNameEvaluatorPlot
                        p
                        group
                        example
                        name
                        evaluator
                func plotF dataF group example name evaluator
                pure plotF
    perGroupExampleNameOrderedDistinct2EvaluatorPlots <-
        rule plotterRulesEvaluatorGroupExampleNameOrderedDistinct2Evaluator $ \func -> do
            trips <- groupsExamplesAndNames
            fmap concat $
                forM trips $ \(group, example, name) ->
                    forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                        dataF <-
                            evaluatedFileForGroupExampleName group example name
                        plotF <-
                            plotterEvaluatorGroupExampleNameOrderedDistinct2EvaluatorPlot
                                p
                                group
                                example
                                name
                                e1
                                e2
                        func plotF dataF group example name e1 e2
                        pure plotF
    plotterRule ~>
        needP
            (concat
                 [ perGroupPlots
                 , perGroupExamplePlots
                 , perGroupExampleEvaluatorPlots
                 , perGroupExampleOrderedDistinct2EvaluatorPlots
                 , perGroupExampleNamePlots
                 , perGroupExampleNameEvaluatorPlots
                 , perGroupExampleNameOrderedDistinct2EvaluatorPlots
                 ])
    pure plotterRule
  where
    rule :: Applicative f => Maybe a -> (a -> f [b]) -> f [b]
    rule Nothing _ = pure []
    rule (Just a) func = func a

data Plotter = Plotter
    { plotterRule :: String
    , plotterRulesEvaluatorGroup :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Rules ())
    , plotterRulesEvaluatorGroupExample :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Rules ())
    , plotterRulesEvaluatorGroupExampleEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Rules ())
    , plotterRulesEvaluatorGroupExampleOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesEvaluatorGroupExampleName :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Rules ())
    , plotterRulesEvaluatorGroupExampleNameEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ())
    , plotterRulesEvaluatorGroupExampleNameOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Evaluator -> Rules ())
    }

plotter :: String -> Plotter
plotter name =
    Plotter
    { plotterRule = name
    , plotterRulesEvaluatorGroup = Nothing
    , plotterRulesEvaluatorGroupExample = Nothing
    , plotterRulesEvaluatorGroupExampleEvaluator = Nothing
    , plotterRulesEvaluatorGroupExampleOrderedDistinct2Evaluator = Nothing
    , plotterRulesEvaluatorGroupExampleName = Nothing
    , plotterRulesEvaluatorGroupExampleNameEvaluator = Nothing
    , plotterRulesEvaluatorGroupExampleNameOrderedDistinct2Evaluator = Nothing
    }

plotterEvaluatorGroupPlot ::
       MonadIO m => Plotter -> GroupName -> m (Path Abs File)
plotterEvaluatorGroupPlot p g = plotterPlotFile p ["per-group"] [g]

plotterEvaluatorGroupExamplePlot ::
       MonadIO m => Plotter -> GroupName -> Example -> m (Path Abs File)
plotterEvaluatorGroupExamplePlot p g e =
    plotterPlotFile p ["per-group-example"] [g, exampleModule e]

plotterEvaluatorGroupExampleEvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupExampleEvaluatorPlot p g e e1 =
    plotterPlotFile
        p
        ["per-group-example-evaluator"]
        [g, exampleModule e, evaluatorName e1]

plotterEvaluatorGroupExampleOrderedDistinct2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupExampleOrderedDistinct2EvaluatorPlot p g e e1 e2 =
    plotterPlotFile
        p
        ["per-group-example-ordered-distinct-evaluators"]
        [g, exampleModule e, evaluatorName e1, evaluatorName e2]

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
        ["per-group-example-name-evaluator"]
        [g, exampleModule e, prettyPrint n, evaluatorName e1]

plotterEvaluatorGroupExampleNameOrderedDistinct2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> ExampleFunction
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupExampleNameOrderedDistinct2EvaluatorPlot p g e n e1 e2 =
    plotterPlotFile
        p
        ["per-group-example-name-ordered-distinct-evaluators"]
        [g, exampleModule e, prettyPrint n, evaluatorName e1, evaluatorName e2]

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
