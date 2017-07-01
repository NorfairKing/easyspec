{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Plots.Plotter
    ( Plotter(..)
    , plotter
    , plotRulesForPlotter
    ) where

import Import hiding (group)

import Data.String
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath (dropExtensions)

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common
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
    perEvaluatorPlots <-
        rule plotterRulesEvaluator $ \func -> do
            forM evaluators $ \evaluator -> do
                dataF <- evaluatedFileForEvaluator evaluator
                plotF <- plotterEvaluatorEvaluatorPlot p evaluator
                func plotF dataF evaluator
                pure plotF
    perOrderedDistinct2EvaluatorPlots <-
        rule plotterRulesOrderedDistinct2Evaluator $ \func ->
            forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                dataF <- evaluatedFileForAllData
                plotF <- plotterEvaluatorOrderedDistinct2EvaluatorPlot p e1 e2
                func plotF dataF e1 e2
                pure plotF
    perGroupPlots <-
        rule plotterRulesGroup $ \func ->
            forM groups $ \group -> do
                dataF <- dataFileForExampleGroup group
                plotF <- plotterEvaluatorGroupPlot p group
                func plotF dataF group
                pure plotF
    perGroupEvaluatorPlots <-
        rule plotterRulesGroupEvaluator $ \func -> do
            forM ((,) <$> groups <*> evaluators) $ \(group, evaluator) -> do
                dataF <- evaluatedFileForGroupEvaluator group evaluator
                plotF <- plotterEvaluatorGroupEvaluatorPlot p group evaluator
                func plotF dataF group evaluator
                pure plotF
    perGroupOrderedDistinct2EvaluatorPlots <-
        rule plotterRulesGroupOrderedDistinct2Evaluator $ \func ->
            fmap concat $
            forM groups $ \group ->
                forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                    dataF <- evaluatedFileForGroup group
                    plotF <-
                        plotterEvaluatorGroupOrderedDistinct2EvaluatorPlot
                            p
                            group
                            e1
                            e2
                    func plotF dataF group e1 e2
                    pure plotF
    perGroupStrategyPlots <-
        rule plotterRulesGroupStrategy $ \func ->
            forM ((,) <$> groups <*> signatureInferenceStrategies) $ \(group, strategy) -> do
                dataF <- evaluatedFileForGroupStrategy group strategy
                plotF <- plotterEvaluatorGroupStrategyPlot p group strategy
                func plotF dataF group strategy
                pure plotF
    perGroupStrategyEvaluatorPlots <-
        rule plotterRulesGroupStrategyEvaluator $ \func -> do
            forM
                ((,,) <$> groups <*> signatureInferenceStrategies <*> evaluators) $ \(group, strategy, evaluator) -> do
                dataF <-
                    evaluatedFileForGroupStrategyEvaluator
                        group
                        strategy
                        evaluator
                plotF <-
                    plotterEvaluatorGroupStrategyEvaluatorPlot
                        p
                        group
                        strategy
                        evaluator
                func plotF dataF group strategy evaluator
                pure plotF
    perGroupStrategyOrderedDistinct2EvaluatorPlots <-
        rule plotterRulesGroupStrategyOrderedDistinct2Evaluator $ \func ->
            fmap concat $
            forM ((,) <$> groups <*> signatureInferenceStrategies) $ \(group, strategy) ->
                forM (orderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                    dataF <- evaluatedFileForGroupStrategy group strategy
                    plotF <-
                        plotterEvaluatorGroupStrategyOrderedDistinct2EvaluatorPlot
                            p
                            group
                            strategy
                            e1
                            e2
                    func plotF dataF group strategy e1 e2
                    pure plotF
    perGroupExamplePlots <-
        rule plotterRulesGroupExample $ \func ->
            forM groupsAndExamples $ \(group, example) -> do
                dataF <- dataFileForExample group example
                plotF <- plotterEvaluatorGroupExamplePlot p group example
                func plotF dataF group example
                pure plotF
    perGroupExampleNamePlots <-
        rule plotterRulesGroupExampleName $ \func -> do
            trips <- groupsExamplesAndNames
            forM trips $ \(group, example, name) -> do
                dataF <- dataFileForExampleAndName group example name
                plotF <-
                    plotterEvaluatorGroupExampleNamePlot p group example name
                func plotF dataF group example name
                pure plotF
    perGroupExampleEvaluatorPlots <-
        rule plotterRulesGroupExampleEvaluator $ \func -> do
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
        rule plotterRulesGroupExampleOrderedDistinct2Evaluator $ \func ->
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
        rule plotterRulesGroupExampleNameEvaluator $ \func -> do
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
        rule plotterRulesGroupExampleNameOrderedDistinct2Evaluator $ \func -> do
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
                 [ perEvaluatorPlots
                 , perOrderedDistinct2EvaluatorPlots
                 , perGroupPlots
                 , perGroupEvaluatorPlots
                 , perGroupOrderedDistinct2EvaluatorPlots
                 , perGroupStrategyPlots
                 , perGroupStrategyEvaluatorPlots
                 , perGroupStrategyOrderedDistinct2EvaluatorPlots
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
    , plotterRulesEvaluator :: Maybe (Path Abs File -> Path Abs File -> Evaluator -> Rules ())
    , plotterRulesOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroup :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Rules ())
    , plotterRulesGroupEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Evaluator -> Rules ())
    , plotterRulesGroupOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupStrategy :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Rules ())
    , plotterRulesGroupStrategyEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Evaluator -> Rules ())
    , plotterRulesGroupStrategyOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupExample :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Rules ())
    , plotterRulesGroupExampleEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Rules ())
    , plotterRulesGroupExampleOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupExampleName :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Rules ())
    , plotterRulesGroupExampleNameEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ())
    , plotterRulesGroupExampleNameOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Evaluator -> Rules ())
    }

instance IsString Plotter where
    fromString = plotter

plotter :: String -> Plotter
plotter name =
    Plotter
    { plotterRule = name
    , plotterRulesEvaluator = Nothing
    , plotterRulesOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroup = Nothing
    , plotterRulesGroupEvaluator = Nothing
    , plotterRulesGroupOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroupStrategy = Nothing
    , plotterRulesGroupStrategyEvaluator = Nothing
    , plotterRulesGroupStrategyOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroupExample = Nothing
    , plotterRulesGroupExampleEvaluator = Nothing
    , plotterRulesGroupExampleOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroupExampleName = Nothing
    , plotterRulesGroupExampleNameEvaluator = Nothing
    , plotterRulesGroupExampleNameOrderedDistinct2Evaluator = Nothing
    }

plotterEvaluatorEvaluatorPlot ::
       MonadIO m => Plotter -> Evaluator -> m (Path Abs File)
plotterEvaluatorEvaluatorPlot p e1 =
    plotterPlotFile p ["per-evaluator"] [evaluatorName e1]

plotterEvaluatorOrderedDistinct2EvaluatorPlot ::
       MonadIO m => Plotter -> Evaluator -> Evaluator -> m (Path Abs File)
plotterEvaluatorOrderedDistinct2EvaluatorPlot p e1 e2 =
    plotterPlotFile
        p
        ["per-ordered-distinct-evaluators"]
        [evaluatorName e1, evaluatorName e2]

plotterEvaluatorGroupPlot ::
       MonadIO m => Plotter -> GroupName -> m (Path Abs File)
plotterEvaluatorGroupPlot p g = plotterPlotFile p ["per-group"] [g]

plotterEvaluatorGroupEvaluatorPlot ::
       MonadIO m => Plotter -> GroupName -> Evaluator -> m (Path Abs File)
plotterEvaluatorGroupEvaluatorPlot p g e1 =
    plotterPlotFile p ["per-group-evaluator"] [g, evaluatorName e1]

plotterEvaluatorGroupOrderedDistinct2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupOrderedDistinct2EvaluatorPlot p g e1 e2 =
    plotterPlotFile
        p
        ["per-group-ordered-distinct-evaluators"]
        [g, evaluatorName e1, evaluatorName e2]

plotterEvaluatorGroupStrategyPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
plotterEvaluatorGroupStrategyPlot p g s =
    plotterPlotFile p ["per-group-strategy"] [g, strategyName s]

plotterEvaluatorGroupStrategyEvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> SignatureInferenceStrategy
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupStrategyEvaluatorPlot p g s e1 =
    plotterPlotFile
        p
        ["per-group-strategy-evaluator"]
        [g, strategyName s, evaluatorName e1]

plotterEvaluatorGroupStrategyOrderedDistinct2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> SignatureInferenceStrategy
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupStrategyOrderedDistinct2EvaluatorPlot p g s e1 e2 =
    plotterPlotFile
        p
        ["per-group-strategy-ordered-distinct-evaluators"]
        [g, strategyName s, evaluatorName e1, evaluatorName e2]

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
    let fileStr = intercalate "-" $ "plot" : comps
    liftIO $ resolveFile pd $ dirStr ++ "/" ++ fileStr ++ ".png"
