module EasySpec.Evaluate.Analyse.Archive where

import Import

import EasySpec.Evaluate.Analyse.Common

import Development.Shake
import Development.Shake.Path

archiveRule :: String
archiveRule = "zip"

archiveRules :: Rules ()
archiveRules = do
    zf <- archiveFile
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
    archiveRule ~> needP [zf]

archiveFile :: MonadIO m => m (Path Abs File)
archiveFile = do
    od <- outDir
    liftIO $ resolveFile od "easyspec-data.tar.gz"
