module EasySpec.Evaluate.Analyse.Common where

import Import

import System.FilePath (dropExtensions)

import EasySpec.Utils

examplesDir :: MonadIO m => m (Path Abs Dir)
examplesDir = liftIO $ resolveDir' "../examples"

tmpDir :: MonadIO m => m (Path Abs Dir)
tmpDir = liftIO $ resolveDir' "tmp"

outDir :: MonadIO m => m (Path Abs Dir)
outDir = liftIO $ resolveDir' "out"

fileInDirWithExtensionAndComponents ::
       MonadIO m
    => m (Path Abs Dir)
    -> String
    -> Path Rel File
    -> [String]
    -> m (Path Abs File)
fileInDirWithExtensionAndComponents genDir ext f comps = do
    dd <- genDir
    let fileStr = intercalate "-" $ dropExtensions (toFilePath f) : comps
    liftIO $ (dd </>) <$> parseRelFile (concat [fileStr, ".", ext])

examples :: MonadIO m => m [Path Rel File]
examples = do
    edir <- examplesDir
    sourcesIn edir

absExampleFile :: MonadIO m => Path Rel File -> m (Path Abs File)
absExampleFile f = (</> f) <$> examplesDir
