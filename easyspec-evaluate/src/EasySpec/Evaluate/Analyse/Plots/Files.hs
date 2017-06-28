{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Evaluate.Evaluator.Types

commonRFile :: MonadIO m => m (Path Abs File)
commonRFile = scriptFile "common.r"

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

pointsPlotForEvaluators ::
       MonadIO m => Evaluator -> Evaluator -> m (Path Abs File)
pointsPlotForEvaluators e1 e2 =
    pngPlotFileWithComponents
        $(mkRelFile "points/global/points-plot")
        [evaluatorName e1, evaluatorName e2]

pointsPlotForEvaluatorsPerExample ::
       MonadIO m => ES.InputSpec -> Evaluator -> Evaluator -> m (Path Abs File)
pointsPlotForEvaluatorsPerExample is e1 e2 =
    pngPlotFileWithComponents
        ($(mkRelDir "points/per-example") </> ES.inputSpecFile is)
        [evaluatorName e1, evaluatorName e2]

pointsPlotForEvaluatorsPerExampleGroup ::
       MonadIO m => String -> Evaluator -> Evaluator -> m (Path Abs File)
pointsPlotForEvaluatorsPerExampleGroup groupName e1 e2 =
    pngPlotFileWithComponents
        $(mkRelFile "points/per-group/group")
        [groupName, evaluatorName e1, evaluatorName e2]

pointsPlotAnalysisScript :: MonadIO m => m (Path Abs File)
pointsPlotAnalysisScript = scriptFile "points.r"

singleEvaluatorBarPlotFileForExampleAndName ::
       MonadIO m
    => ES.InputSpec
    -> ES.EasyQName
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
        ($(mkRelDir "single-evaluator-per-example-box") </> ES.inputSpecFile is)
        [evaluatorName ev]

singleEvaluatorAverageBoxAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorAverageBoxAnalysisScript =
    scriptFile "single_evaluator_boxplot_average.r"

singleEvaluatorAverageGlobalBoxPlotFileForExample ::
       MonadIO m => Evaluator -> m (Path Abs File)
singleEvaluatorAverageGlobalBoxPlotFileForExample ev =
    pngPlotFileWithComponents
        ($(mkRelDir "single-evaluator-global-box") </> $(mkRelFile "global"))
        [evaluatorName ev]

singleEvaluatorAverageBoxGlobalAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorAverageBoxGlobalAnalysisScript =
    scriptFile "single_evaluator_boxplot_average_global.r"
