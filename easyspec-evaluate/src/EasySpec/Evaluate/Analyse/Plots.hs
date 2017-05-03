{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots
    ( plotsRule
    , plotsRules
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

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
    plotsFs <- concat <$> mapM plotsRulesForExample es
    plotsRule ~> needP plotsFs

plotsRulesForExample :: Path Rel File -> Rules [Path Abs File]
plotsRulesForExample sourceF = do
    absSourceF <- absExampleFile sourceF
    names <- namesInSource absSourceF
    fmap concat $ forM names $ plotsRulesForExampleAndName sourceF

plotsRulesForExampleAndName ::
       Path Rel File -> ES.EasyName -> Rules [Path Abs File]
plotsRulesForExampleAndName sourceF name =
    forM evaluators $ perExampleNameAndEvaluatorBarPlotFor sourceF name

perExampleNameAndEvaluatorBarPlotFor ::
       Path Rel File -> ES.EasyName -> Evaluator -> Rules (Path Abs File)
perExampleNameAndEvaluatorBarPlotFor sourceF name evaluator = do
    singleEvaluatorBarScript <- singleEvaluatorBarAnalysisScript
    dataFile <- dataFileForExampleAndName sourceF name
    runtimePlotFile <-
        singleEvaluatorBarPlotFileForExampleAndName sourceF name evaluator
    runtimePlotFile $%> do
        needP [singleEvaluatorBarScript, dataFile]
        cmd
            "Rscript"
            (toFilePath singleEvaluatorBarScript)
            (toFilePath dataFile)
            (toFilePath runtimePlotFile)
            (toFilePath sourceF)
            (prettyPrint name)
            (evaluatorName evaluator)
    pure runtimePlotFile
