{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data

import Development.Shake
import Development.Shake.Path

plotsRules :: Rules [Path Abs File]
plotsRules = concat <$> forExamples plotsRulesForExample

plotsDir :: MonadIO m => m (Path Abs Dir)
plotsDir = (</> $(mkRelDir "plots")) <$> tmpDir

pngPlotFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
pngPlotFileWithComponents = fileInDirWithExtensionAndComponents plotsDir "png"

singleEvaluatorBarPlotFileForExampleAndName ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> Evaluator a
    -> m (Path Abs File)
singleEvaluatorBarPlotFileForExampleAndName file name ev =
    pngPlotFileWithComponents
        file
        ["runtime", prettyPrint name, evaluatorName ev]

plotsRulesForExample :: Path Rel File -> Rules [Path Abs File]
plotsRulesForExample sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    fmap concat $ forM names $ plotsRulesForExampleAndName sourceF

scriptFile :: MonadIO m => String -> m (Path Abs File)
scriptFile fname = liftIO $ resolveFile' $ "rscripts/" ++ fname

singleEvaluatorBarAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorBarAnalysisScript = scriptFile "single_evaluator_bar.r"

plotsRulesForExampleAndName ::
       Path Rel File -> ES.EasyName -> Rules [Path Abs File]
plotsRulesForExampleAndName sourceF name = do
    singleEvaluatorBarScript <- singleEvaluatorBarAnalysisScript
    dataFile <- dataFileForExampleAndName sourceF name
    forM evaluators $ \(AnyEvaluator evaluator) -> do
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
