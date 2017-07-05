module EasySpec.Evaluate.Analyse.Archive where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files

archiveRule :: String
archiveRule = "zip"

archiveRules :: Rules ()
archiveRules = do
    zf <- archiveFile
    pd <- plotsDir
    dd <- dataDir
    zf $%> do
        dfs <- liftIO $ snd <$> listDirRecur dd
        pfs <- liftIO $ snd <$> listDirRecur pd
        let fs = pfs ++ dfs
        needP fs
        cmd
            (Cwd $ toFilePath pd)
            (EchoStdout False)
            "tar"
            "cvzf"
            (toFilePath zf)
            (map toFilePath $ mapMaybe (stripDir pd) fs)
    archiveRule ~> needP [zf]

archiveFile :: MonadIO m => m (Path Abs File)
archiveFile = do
    od <- outDir
    liftIO $ resolveFile od "easyspec-data.tar.gz"
