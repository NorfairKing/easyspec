{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

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
    pngPlotFileWithComponents $(mkRelFile "lines-plot") [evaluatorName ev]

linesPlotAnalysisScript :: MonadIO m => m (Path Abs File)
linesPlotAnalysisScript = scriptFile "lines.r"

singleEvaluatorBarPlotFileForExampleAndName ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> Evaluator
    -> m (Path Abs File)
singleEvaluatorBarPlotFileForExampleAndName file name ev =
    pngPlotFileWithComponents file [prettyPrint name, evaluatorName ev]

singleEvaluatorBarAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorBarAnalysisScript = scriptFile "single_evaluator_bar.r"

singleEvaluatorAverageBoxPlotFileForExample ::
       MonadIO m => Path Rel File -> Evaluator -> m (Path Abs File)
singleEvaluatorAverageBoxPlotFileForExample file ev =
    pngPlotFileWithComponents file ["average", evaluatorName ev]

singleEvaluatorAverageBoxAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorAverageBoxAnalysisScript =
    scriptFile "single_evaluator_boxplot_average.r"
