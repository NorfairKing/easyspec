module EasySpec.Evaluate.Analyse.Archive where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common

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
            (EchoStdout False)
            "tar"
            "cvzf"
            (toFilePath zf)
            (map toFilePath $ mapMaybe (stripDir td) fs)
    archiveRule ~> needP [zf]

archiveFile :: MonadIO m => m (Path Abs File)
archiveFile = do
    od <- outDir
    liftIO $ resolveFile od "easyspec-data.tar.gz"
