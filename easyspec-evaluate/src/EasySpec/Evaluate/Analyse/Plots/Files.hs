{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.Files where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath (dropExtensions)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

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

exampleModule :: Example -> String
exampleModule = map go . dropExtensions . toFilePath . ES.inputSpecFile
  where
    go :: Char -> Char
    go '/' = '.'
    go c = c
