module EasySpec.Evaluate.Analyse.Archive where

import Import

import EasySpec.Evaluate.Analyse.Common

import Development.Shake
import Development.Shake.Path

analysisZipFile :: MonadIO m => m (Path Abs File)
analysisZipFile = do
    od <- outDir
    liftIO $ resolveFile od "easyspec-data.tar.gz"

analysisZipFileRules :: Rules (Path Abs File)
analysisZipFileRules = do
    zf <- analysisZipFile
    td <- tmpDir
    zf $%> do
        fs <- liftIO $ snd <$> listDirRecur td
        needP fs
        cmd
            (Cwd $ toFilePath td)
            "tar"
            "cvzf"
            (toFilePath zf)
            (map toFilePath $ mapMaybe (stripDir td) fs)
    pure zf
