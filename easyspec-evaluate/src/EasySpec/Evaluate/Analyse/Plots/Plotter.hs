{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module EasySpec.Evaluate.Analyse.Plots.Plotter
    ( Plotter(..)
    , plotter
    , plotRulesForPlotter
    , CartPlotter(..)
    , EvaluatedCartPlotter
    , evaluatedCartRule
    , OrderedDistinct(..)
    , UnorderedDistinct(..)
    , standardisedEvaluatedPlotruleFor
    , Cart
    , EvaluatedData
    ) where

import Import hiding (group)

import Data.Proxy
import Data.String
import Language.Haskell.Exts.Pretty (prettyPrint)

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Content
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R
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
        rule plotterRulesEvaluator $ \func ->
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
        rule plotterRulesGroupEvaluator $ \func ->
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
    perGroupUnorderedDistinct2EvaluatorPlots <-
        rule plotterRulesGroupUnorderedDistinct2Evaluator $ \func ->
            fmap concat $
            forM groups $ \group ->
                forM (unorderedCombinationsWithoutSelfCombinations evaluators) $ \(e1, e2) -> do
                    dataF <- evaluatedFileForGroup group
                    plotF <-
                        plotterEvaluatorGroupUnorderedDistinct2EvaluatorPlot
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
        rule plotterRulesGroupStrategyEvaluator $ \func ->
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
        rule plotterRulesGroupExampleEvaluator $ \func ->
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
    rawGroupStrategyPlots <-
        rule plotterRulesRawGroupStrategy $ \func ->
            forM ((,) <$> groups <*> signatureInferenceStrategies) $ \(group, strategy) -> do
                plotF <- plotterEvaluatorRawGroupStrategy p group strategy
                func
                    plotF
                    (rawDataFromGroupStrategy group strategy)
                    group
                    strategy
                pure plotF
    rawGroupExampleNameStrategyPlots <-
        rule plotterRulesRawGroupExampleNameStrategy $ \func -> do
            quads <- groupsExamplesNamesAndStrategies
            forM quads $ \(group, example, name, strategy) -> do
                plotF <-
                    plotterEvaluatorRawGroupExampleNameStrategy
                        p
                        group
                        example
                        name
                        strategy
                func
                    plotF
                    (rawDataFrom group example name strategy)
                    group
                    example
                    name
                    strategy
                pure plotF
    plotterRule ~>
        needP
            (concat
                 [ perEvaluatorPlots
                 , perOrderedDistinct2EvaluatorPlots
                 , perGroupPlots
                 , perGroupEvaluatorPlots
                 , perGroupOrderedDistinct2EvaluatorPlots
                 , perGroupUnorderedDistinct2EvaluatorPlots
                 , perGroupStrategyPlots
                 , perGroupStrategyEvaluatorPlots
                 , perGroupStrategyOrderedDistinct2EvaluatorPlots
                 , perGroupExamplePlots
                 , perGroupExampleEvaluatorPlots
                 , perGroupExampleOrderedDistinct2EvaluatorPlots
                 , perGroupExampleNamePlots
                 , perGroupExampleNameEvaluatorPlots
                 , perGroupExampleNameOrderedDistinct2EvaluatorPlots
                 , rawGroupStrategyPlots
                 , rawGroupExampleNameStrategyPlots
                 ])
    pure plotterRule
  where
    rule :: Applicative f => Maybe a -> (a -> f [b]) -> f [b]
    rule Nothing _ = pure []
    rule (Just a) func = func a

-- Something that makes a plot for every option of a, with an input of b
data CartPlotter a b = CartPlotter
    { cartPlotterName :: String
    , cartPlotterFunc :: Path Abs File -> Action b -> a -> Rules ()
    }

type EvaluatedCartPlotter a = CartPlotter a (Path Abs File)

cartPlotterRule ::
       forall a b. Cart a
    => CartPlotter a b
    -> String
cartPlotterRule cp = intercalate "-" $ cartPlotterName cp : ruleComps (Proxy @a)

evaluatedCartRule ::
       (Cart a, EvaluatedData a) => EvaluatedCartPlotter a -> Rules String
evaluatedCartRule cp = do
    options <- getAllOptions
    plotFs <-
        forM options $ \option -> do
            plotF <- plotFileFor cp option
            cartPlotterFunc cp plotF (getDataFileFor option) option
            pure plotF
    let rule = cartPlotterRule cp
    rule ~> needP plotFs
    pure rule

plotFileFor :: Cart a => CartPlotter a b -> a -> Rules (Path Abs File)
plotFileFor cp a = do
    pd <- plotsDir
    let (mFileComp, dirComps) =
            case fileComps a of
                [] -> (Nothing, [])
                (x:rs) -> (Just x, rs)
    let dirStr = intercalate "/" $ cartPlotterRule cp : dirComps
    let fileStr =
            intercalate "-" $
            case mFileComp of
                Nothing -> ["plot"]
                Just fileComp -> ["plot", fileComp]
    liftIO $ resolveFile pd $ dirStr ++ "/" ++ fileStr ++ ".png"

standardisedEvaluatedPlotruleFor ::
       forall a. Cart a
    => Action (Path Abs File)
    -> Path Abs File
    -> Action (Path Abs File)
    -> a
    -> Rules ()
standardisedEvaluatedPlotruleFor genScriptF plotF genDataF option =
    plotF $%> do
        dependencies option
        dataF <- genDataF
        needP [dataF]
        scriptF <- genScriptF
        rscript scriptF $
            [ toFilePath dataF
            , toFilePath plotF
            , intercalate "-" $ ruleComps $ Proxy @a
            ] ++
            plotArgs option

class Cart a where
    getAllOptions :: Rules [a]
    fileComps :: a -> [String]
    ruleComps :: Proxy a -> [String]
    dependencies :: a -> Action ()
    dependencies _ = pure ()
    plotArgs :: a -> [String] -- Warning: changing this instance function breaks cross-script modularity
    plotArgs = fileComps

instance Cart GroupName where
    getAllOptions = pure groups
    fileComps g = [g]
    ruleComps Proxy = ["group"]

instance Cart (GroupName, Example) where
    getAllOptions = pure groupsAndExamples
    fileComps (g, e) = [g, exampleModule e]
    ruleComps Proxy = ["group", "example"]

instance Cart (GroupName, Example, ExampleFunction) where
    getAllOptions = groupsExamplesAndNames
    fileComps (g, e, n) = [g, exampleModule e, prettyPrint n]
    ruleComps Proxy = ["group", "example", "name"]

instance Cart Evaluator where
    getAllOptions = pure evaluators
    fileComps e = [evaluatorName e]
    ruleComps Proxy = ["evaluator"]
    dependencies = dependOnEvaluator
    plotArgs ev = [evaluatorName ev, prettyIndication $ evaluatorIndication ev]

data OrderedDistinct a =
    OrderedDistinct a
                    a

instance Cart a => Cart (OrderedDistinct a) where
    getAllOptions =
        (map (uncurry OrderedDistinct) .
         orderedCombinationsWithoutSelfCombinations) <$>
        getAllOptions
    fileComps (OrderedDistinct a b) = fileComps a ++ fileComps b
    ruleComps Proxy = ["ordered", "distinct"] ++ ruleComps (Proxy @a)
    dependencies (OrderedDistinct a b) = do
        dependencies a
        dependencies b
    plotArgs (OrderedDistinct a b) = plotArgs a ++ plotArgs b

data UnorderedDistinct a =
    UnorderedDistinct a
                      a

instance Cart a => Cart (UnorderedDistinct a) where
    getAllOptions =
        (map (uncurry UnorderedDistinct) .
         unorderedCombinationsWithoutSelfCombinations) <$>
        getAllOptions
    fileComps (UnorderedDistinct a b) = fileComps a ++ fileComps b
    ruleComps Proxy = ["unordered", "distinct"] ++ ruleComps (Proxy @a)
    dependencies (UnorderedDistinct a b) = do
        dependencies a
        dependencies b
    plotArgs (UnorderedDistinct a b) = plotArgs a ++ plotArgs b

instance Cart SignatureInferenceStrategy where
    getAllOptions = pure signatureInferenceStrategies
    fileComps s = [strategyName s]
    ruleComps Proxy = ["strategy"]

instance (Cart a, Cart b) => Cart (a, b) where
    getAllOptions = do
        as <- getAllOptions
        bs <- getAllOptions
        pure $ (,) <$> as <*> bs
    fileComps (a, b) = fileComps a ++ fileComps b
    ruleComps Proxy = ruleComps (Proxy @a) ++ ruleComps (Proxy @b)
    dependencies (a, b) = do
        dependencies a
        dependencies b
    plotArgs (a, b) = plotArgs a ++ plotArgs b

instance (Cart a, Cart b, Cart c) => Cart (a, b, c) where
    getAllOptions = do
        as <- getAllOptions
        bs <- getAllOptions
        cs <- getAllOptions
        pure $ (,,) <$> as <*> bs <*> cs
    fileComps (a, b, c) = fileComps a ++ fileComps b ++ fileComps c
    ruleComps Proxy =
        ruleComps (Proxy @a) ++ ruleComps (Proxy @b) ++ ruleComps (Proxy @c)
    dependencies (a, b, c) = do
        dependencies a
        dependencies b
        dependencies c
    plotArgs (a, b, c) = plotArgs a ++ plotArgs b ++ plotArgs c

class EvaluatedData a where
    getDataFileFor :: a -> Action (Path Abs File)

instance EvaluatedData (GroupName, UnorderedDistinct Evaluator) where
    getDataFileFor (g, _) = evaluatedFileForGroup g

instance EvaluatedData ((GroupName, Example), Evaluator) where
    getDataFileFor ((g, e), ev) = evaluatedFileForGroupExampleEvaluator g e ev

instance EvaluatedData Evaluator where
    getDataFileFor = evaluatedFileForEvaluator

data Plotter = Plotter
    { plotterRule :: String
    , plotterRulesEvaluator :: Maybe (Path Abs File -> Path Abs File -> Evaluator -> Rules ())
    , plotterRulesOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroup :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Rules ())
    , plotterRulesGroupEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Evaluator -> Rules ())
    , plotterRulesGroupOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupUnorderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupStrategy :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Rules ())
    , plotterRulesGroupStrategyEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Evaluator -> Rules ())
    , plotterRulesGroupStrategyOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> SignatureInferenceStrategy -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupExample :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Rules ())
    , plotterRulesGroupExampleEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Rules ())
    , plotterRulesGroupExampleOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesGroupExampleName :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Rules ())
    , plotterRulesGroupExampleNameEvaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Rules ())
    , plotterRulesGroupExampleNameOrderedDistinct2Evaluator :: Maybe (Path Abs File -> Path Abs File -> GroupName -> Example -> ExampleFunction -> Evaluator -> Evaluator -> Rules ())
    , plotterRulesRawGroupStrategy :: Maybe (Path Abs File -> Action [EvaluationInputPoint] -> GroupName -> SignatureInferenceStrategy -> Rules ())
    , plotterRulesRawGroupExampleNameStrategy :: Maybe (Path Abs File -> Action EvaluationInputPoint -> GroupName -> Example -> ExampleFunction -> SignatureInferenceStrategy -> Rules ())
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
    , plotterRulesGroupUnorderedDistinct2Evaluator = Nothing
    , plotterRulesGroupStrategy = Nothing
    , plotterRulesGroupStrategyEvaluator = Nothing
    , plotterRulesGroupStrategyOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroupExample = Nothing
    , plotterRulesGroupExampleEvaluator = Nothing
    , plotterRulesGroupExampleOrderedDistinct2Evaluator = Nothing
    , plotterRulesGroupExampleName = Nothing
    , plotterRulesGroupExampleNameEvaluator = Nothing
    , plotterRulesGroupExampleNameOrderedDistinct2Evaluator = Nothing
    , plotterRulesRawGroupStrategy = Nothing
    , plotterRulesRawGroupExampleNameStrategy = Nothing
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

plotterEvaluatorGroupUnorderedDistinct2EvaluatorPlot ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Evaluator
    -> Evaluator
    -> m (Path Abs File)
plotterEvaluatorGroupUnorderedDistinct2EvaluatorPlot p g e1 e2 =
    plotterPlotFile
        p
        ["per-group-unordered-distinct-evaluators"]
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

plotterEvaluatorRawGroupExampleNameStrategy ::
       MonadIO m
    => Plotter
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
plotterEvaluatorRawGroupExampleNameStrategy p g e n s =
    plotterPlotFile
        p
        ["raw-per-group-example-name-strategy"]
        [g, exampleModule e, prettyPrint n, strategyName s]

plotterEvaluatorRawGroupStrategy ::
       MonadIO m
    => Plotter
    -> GroupName
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
plotterEvaluatorRawGroupStrategy p g s =
    plotterPlotFile p ["raw-per-group-strategy"] [g, strategyName s]

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
