{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.Files where

import Import

import System.FilePath (dropExtensions)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Types

commonRFile :: MonadIO m => m (Path Abs File)
commonRFile = scriptFile "common.r"

plotsDir :: MonadIO m => m (Path Abs Dir)
plotsDir = (</> $(mkRelDir "plots")) <$> tmpDir

scriptFile :: MonadIO m => String -> m (Path Abs File)
scriptFile fname = liftIO $ resolveFile' $ "rscripts/" ++ fname

pdfPlotFileWithComponents ::
       MonadIO m => Path Abs File -> [String] -> m (Path Abs File)
pdfPlotFileWithComponents = fileInDirWithExtensionAndComponents plotsDir "pdf"

exampleModule :: Example -> String
exampleModule = map go . dropExtensions . toFilePath . ES.inputSpecFile
  where
    -- TODO: This method of converting paths to module names is just
    -- incorrect.
    go :: Char -> Char
    go '/' = '.'
    go c = c
