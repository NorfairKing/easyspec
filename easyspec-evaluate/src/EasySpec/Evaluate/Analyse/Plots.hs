{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots
    ( plotsRule
    , plotsRules
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files

plotsRule :: String
plotsRule = "plots"

plotsRules :: Rules ()
plotsRules = do
    es <- examples
    allDataPlotsFs <- plotsRulesForAllData
    plotsFs <- concat <$> mapM plotsRulesForExample es
    plotsRule ~> needP (allDataPlotsFs ++ plotsFs)

plotsRulesForAllData :: Rules [Path Abs File]
plotsRulesForAllData = mapM plotsRulesForLinesPlotWithEvaluator evaluators

plotsRulesForLinesPlotWithEvaluator :: Evaluator -> Rules (Path Abs File)
plotsRulesForLinesPlotWithEvaluator ev = do
    plotF <- linesPlotForEvaluator ev
    plotF $%> do
        dataF <- allDataFile
        scriptF <- linesPlotAnalysisScript
        needP [dataF, scriptF]
        cmd
            "Rscript"
            (toFilePath scriptF)
            (toFilePath dataF)
            (toFilePath plotF)
            (evaluatorName ev)
    pure plotF

plotsRulesForExample :: ES.InputSpec -> Rules [Path Abs File]
plotsRulesForExample is = do
    names <- namesInSource is
    bars <- fmap concat $ forM names $ plotsRulesForExampleAndName is
    boxes <-
        forM evaluators $ \ev -> perExampleAndEvaluatorAverageBoxPlotFor is ev
    pure $ bars ++ boxes

plotsRulesForExampleAndName ::
       ES.InputSpec -> ES.EasyName -> Rules [Path Abs File]
plotsRulesForExampleAndName is name =
    forM evaluators $ perExampleNameAndEvaluatorBarPlotFor is name

perExampleNameAndEvaluatorBarPlotFor ::
       ES.InputSpec -> ES.EasyName -> Evaluator -> Rules (Path Abs File)
perExampleNameAndEvaluatorBarPlotFor is name evaluator = do
    plotFile <- singleEvaluatorBarPlotFileForExampleAndName is name evaluator
    plotFile $%> do
        singleEvaluatorBarScript <- singleEvaluatorBarAnalysisScript
        dataFile <- dataFileForExampleAndName is name
        needP [singleEvaluatorBarScript, dataFile]
        cmd
            "Rscript"
            (toFilePath singleEvaluatorBarScript)
            (toFilePath dataFile)
            (toFilePath plotFile)
            (toFilePath $ ES.inputSpecBaseDir is)
            (toFilePath $ ES.inputSpecFile is)
            (prettyPrint name)
            (evaluatorName evaluator)
    pure plotFile

perExampleAndEvaluatorAverageBoxPlotFor ::
       ES.InputSpec -> Evaluator -> Rules (Path Abs File)
perExampleAndEvaluatorAverageBoxPlotFor is evaluator = do
    plotF <- singleEvaluatorAverageBoxPlotFileForExample is evaluator
    plotF $%> do
        scriptF <- singleEvaluatorAverageBoxAnalysisScript
        dataF <- dataFileForExample is
        needP [scriptF, dataF]
        cmd
            "Rscript"
            (toFilePath scriptF)
            (toFilePath dataF)
            (toFilePath plotF)
            (toFilePath $ ES.inputSpecBaseDir is)
            (toFilePath $ ES.inputSpecFile is)
            (evaluatorName evaluator)
    pure plotF
