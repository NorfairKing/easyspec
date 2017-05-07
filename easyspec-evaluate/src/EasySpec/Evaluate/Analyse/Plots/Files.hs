{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse.Types as ES

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common

plotsDir :: MonadIO m => m (Path Abs Dir)
plotsDir = (</> $(mkRelDir "plots")) <$> tmpDir

scriptFile :: MonadIO m => String -> m (Path Abs File)
scriptFile fname = liftIO $ resolveFile' $ "rscripts/" ++ fname

pngPlotFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
pngPlotFileWithComponents = fileInDirWithExtensionAndComponents plotsDir "png"

linesPlotForEvaluator :: MonadIO m => Evaluator -> m (Path Abs File)
linesPlotForEvaluator ev =
    pngPlotFileWithComponents $(mkRelFile "lines/lines-plot") [evaluatorName ev]

linesPlotAnalysisScript :: MonadIO m => m (Path Abs File)
linesPlotAnalysisScript = scriptFile "lines.r"

singleEvaluatorBarPlotFileForExampleAndName ::
       MonadIO m
    => ES.InputSpec
    -> ES.EasyName
    -> Evaluator
    -> m (Path Abs File)
singleEvaluatorBarPlotFileForExampleAndName is name ev =
    pngPlotFileWithComponents
        ($(mkRelDir "single-evaluator-bar") </> ES.inputSpecFile is)
        [prettyPrint name, evaluatorName ev]

singleEvaluatorBarAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorBarAnalysisScript = scriptFile "single_evaluator_bar.r"

singleEvaluatorAverageBoxPlotFileForExample ::
       MonadIO m => ES.InputSpec -> Evaluator -> m (Path Abs File)
singleEvaluatorAverageBoxPlotFileForExample is ev =
    pngPlotFileWithComponents
        ($(mkRelDir "single-evaluator-box") </> ES.inputSpecFile is)
        ["average", evaluatorName ev]

singleEvaluatorAverageBoxAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorAverageBoxAnalysisScript =
    scriptFile "single_evaluator_boxplot_average.r"
