{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EasySpec.Evaluate.Analyse.Plots.Plotter
    ( CartPlotter(..)
    , EvaluatedCartPlotter
    , RawCartPlotter
    , evaluatedCartRule
    , plotFileFor
    , cartFile
    , granularityStr
    , rawCartRule
    , Pair(..)
    , OrderedDistinct(..)
    , UnorderedDistinct(..)
    , IndepDepPairEvaluator(..)
    , standardisedEvaluatedPlotruleFor
    , GroupAndExample(..)
    , GroupAndExampleAndName(..)
    , Cart(..)
    , EvaluatedData
    ) where

import Import hiding (group)

import Data.Proxy
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
--
-- Something that makes a plot for every option of a, with an input of b
data CartPlotter a b = CartPlotter
    { cartPlotterName :: String
    , cartPlotterFunc :: Path Abs File -> Action b -> a -> Rules ()
    }

type EvaluatedCartPlotter a = CartPlotter a (Path Abs File)

type RawCartPlotter a = CartPlotter a [EvaluationInputPoint]

cartPlotterRule ::
       forall a b. Cart a
    => CartPlotter a b
    -> String
cartPlotterRule cp = intercalate "-" $ cartPlotterName cp : ruleComps (Proxy @a)

evaluatedCartRule ::
       (Cart a, EvaluatedData a) => EvaluatedCartPlotter a -> Rules String
evaluatedCartRule = cartRuleWith getDataFileFor

plotFileFor :: (MonadIO m, Cart a) => CartPlotter a b -> a -> m (Path Abs File)
plotFileFor cp = cartFile "pdf" plotsDir (cartPlotterRule cp) "plot"

cartFile ::
       (MonadIO m, Cart a)
    => String
    -> m (Path Abs Dir)
    -> String
    -> String
    -> a
    -> m (Path Abs File)
cartFile ext genDir extraDirPrefix extraFilePrefix option = do
    pd <- genDir
    let (mFileComp, dirComps) =
            case fileComps option of
                [] -> (Nothing, [])
                (x:rs) -> (Just x, rs)
    let dirStr = intercalate "/" $ extraDirPrefix : dirComps
    let fileStr =
            intercalate
                "-"
                (extraFilePrefix :
                 case mFileComp of
                     Nothing -> []
                     Just fileComp -> [fileComp])
    liftIO $ resolveFile pd $ dirStr ++ "/" ++ fileStr ++ "." ++ ext

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
            [toFilePath dataF, toFilePath plotF, granularityStr $ Proxy @a] ++
            plotArgs option

granularityStr ::
       forall a. Cart a
    => Proxy a
    -> String
granularityStr Proxy = intercalate "-" $ ruleComps $ Proxy @a

rawCartRule :: (Cart a, RawData a) => RawCartPlotter a -> Rules String
rawCartRule = cartRuleWith getRawDataFor

cartRuleWith :: Cart a => (a -> Action b) -> CartPlotter a b -> Rules String
cartRuleWith dataGetter cp = do
    options <- getAllOptions
    plotFs <-
        forM options $ \option -> do
            plotF <- plotFileFor cp option
            cartPlotterFunc cp plotF (dataGetter option) option
            pure plotF
    let rule = cartPlotterRule cp
    rule ~> needP plotFs
    pure rule

data GroupAndExample =
    GE GroupName
       Example

data GroupAndExampleAndName =
    GEN GroupName
        Example
        ExampleFunction

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

instance Cart GroupAndExample where
    getAllOptions = pure $ map (uncurry GE) groupsAndExamples
    fileComps (GE g e) = [g, exampleModule e]
    ruleComps Proxy = ["group", "example"]

instance Cart GroupAndExampleAndName where
    getAllOptions = map (uncurry3 GEN) <$> groupsExamplesAndNames
    fileComps (GEN g e n) = [g, exampleModule e, prettyPrint n]
    ruleComps Proxy = ["group", "example", "name"]

instance Cart Evaluator where
    getAllOptions = pure evaluators
    fileComps e = [evaluatorName e]
    ruleComps Proxy = ["evaluator"]
    dependencies = dependOnEvaluator
    plotArgs ev = [evaluatorName ev, prettyIndication $ evaluatorIndication ev]

data Pair a =
    Pair a
         a
    deriving (Show, Eq, Generic)

instance Cart a => Cart (Pair a) where
    getAllOptions = do
        as <- getAllOptions
        pure $ Pair <$> as <*> as
    fileComps (Pair a b) = fileComps a ++ fileComps b
    ruleComps Proxy = ["pair"] ++ ruleComps (Proxy @a)
    dependencies (Pair a b) = do
        dependencies a
        dependencies b
    plotArgs (Pair a b) = plotArgs a ++ plotArgs b

newtype UnorderedDistinct a =
    UnorderedDistinct (Pair a)
    deriving (Show, Eq, Generic)

instance Cart a => Cart (UnorderedDistinct a) where
    getAllOptions =
        (map (UnorderedDistinct . uncurry Pair) .
         unorderedCombinationsWithoutSelfCombinations) <$>
        getAllOptions
    fileComps (UnorderedDistinct p) = fileComps p
    ruleComps Proxy = ["unordered", "distinct"] ++ ruleComps (Proxy @(Pair a))
    dependencies (UnorderedDistinct p) = dependencies p
    plotArgs (UnorderedDistinct p) = plotArgs p

newtype OrderedDistinct a =
    OrderedDistinct (Pair a)
    deriving (Show, Eq, Generic)

instance Cart a => Cart (OrderedDistinct a) where
    getAllOptions =
        (map (OrderedDistinct . uncurry Pair) .
         orderedCombinationsWithoutSelfCombinations) <$>
        getAllOptions
    fileComps (OrderedDistinct p) = fileComps p
    ruleComps Proxy = ["ordered", "distinct"] ++ ruleComps (Proxy @(Pair a))
    dependencies (OrderedDistinct p) = dependencies p
    plotArgs (OrderedDistinct p) = plotArgs p

newtype IndepDepPairEvaluator =
    IndepDepPairEvaluator (Pair Evaluator)

instance Cart IndepDepPairEvaluator where
    getAllOptions = do
        unorderedPairs <- getAllOptions
        pure $
            catMaybes $ do
                UnorderedDistinct (Pair a b) <- unorderedPairs
                pure $
                    if evaluatorIndication a == Input
                        then Just $ IndepDepPairEvaluator (Pair a b)
                        else Nothing
    fileComps (IndepDepPairEvaluator p) = fileComps p
    ruleComps Proxy =
        ["independent", "dependent"] ++ ruleComps (Proxy @(Pair Evaluator))
    dependencies (IndepDepPairEvaluator p) = dependencies p
    plotArgs (IndepDepPairEvaluator p) = plotArgs p

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

instance (Cart a, Cart b, Cart c, Cart d) => Cart (a, b, c, d) where
    getAllOptions = do
        as <- getAllOptions
        bs <- getAllOptions
        cs <- getAllOptions
        ds <- getAllOptions
        pure $ (,,,) <$> as <*> bs <*> cs <*> ds
    fileComps (a, b, c, d) =
        fileComps a ++ fileComps b ++ fileComps c ++ fileComps d
    ruleComps Proxy =
        ruleComps (Proxy @a) ++
        ruleComps (Proxy @b) ++ ruleComps (Proxy @c) ++ ruleComps (Proxy @d)
    dependencies (a, b, c, d) = do
        dependencies a
        dependencies b
        dependencies c
        dependencies d
    plotArgs (a, b, c, d) = plotArgs a ++ plotArgs b ++ plotArgs c ++ plotArgs d

class EvaluatedData a where
    getDataFileFor :: a -> Action (Path Abs File)

instance EvaluatedData (OrderedDistinct Evaluator) where
    getDataFileFor _ = evaluatedFileForAllData

instance EvaluatedData a => EvaluatedData (a, OrderedDistinct b) where
    getDataFileFor (a, _) = getDataFileFor a

instance EvaluatedData (a, b) => EvaluatedData (a, b, OrderedDistinct c) where
    getDataFileFor (a, b, _) = getDataFileFor (a, b)

instance EvaluatedData (a, b, c) =>
         EvaluatedData (a, b, c, OrderedDistinct d) where
    getDataFileFor (a, b, c, _) = getDataFileFor (a, b, c)

instance EvaluatedData (UnorderedDistinct Evaluator) where
    getDataFileFor _ = evaluatedFileForAllData

instance EvaluatedData a => EvaluatedData (a, UnorderedDistinct b) where
    getDataFileFor (g, _) = getDataFileFor g

instance EvaluatedData (a, b) => EvaluatedData (a, b, UnorderedDistinct c) where
    getDataFileFor (a, b, _) = getDataFileFor (a, b)

instance EvaluatedData (a, b, c) =>
         EvaluatedData (a, b, c, UnorderedDistinct d) where
    getDataFileFor (a, b, c, _) = getDataFileFor (a, b, c)

instance EvaluatedData a => EvaluatedData (a, IndepDepPairEvaluator) where
    getDataFileFor (g, _) = getDataFileFor g

instance EvaluatedData (a, b) =>
         EvaluatedData (a, b, IndepDepPairEvaluator) where
    getDataFileFor (a, b, _) = getDataFileFor (a, b)

instance EvaluatedData GroupName where
    getDataFileFor = evaluatedFileForGroup

instance EvaluatedData Evaluator where
    getDataFileFor = evaluatedFileForEvaluator

instance EvaluatedData (GroupName, SignatureInferenceStrategy) where
    getDataFileFor (g, s) = evaluatedFileForGroupStrategy g s

instance EvaluatedData (GroupName, Evaluator) where
    getDataFileFor (g, e) = evaluatedFileForGroupEvaluator g e

instance EvaluatedData GroupAndExample where
    getDataFileFor (GE g e) = evaluatedFileForGroupExample g e

instance EvaluatedData (GroupAndExample, Evaluator) where
    getDataFileFor (GE g e, ev) = evaluatedFileForGroupExampleEvaluator g e ev

instance EvaluatedData (GroupAndExampleAndName, Evaluator) where
    getDataFileFor (GEN g e n, ev) =
        evaluatedFileForGroupExampleNameEvaluator g e n ev

class RawData a where
    getRawDataFor :: a -> Action [EvaluationInputPoint]

instance RawData (GroupName, SignatureInferenceStrategy) where
    getRawDataFor (g, s) = rawDataFromGroupStrategy g s

instance RawData (GroupAndExampleAndName, SignatureInferenceStrategy) where
    getRawDataFor (GEN g e n, s) = (: []) <$> rawDataFrom g e n s
