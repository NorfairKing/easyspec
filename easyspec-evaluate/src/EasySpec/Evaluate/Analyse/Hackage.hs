{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Hackage where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common

hackageRule :: String
hackageRule = "hackage"

hackageRules :: Rules ()
hackageRules = do
    packageRules <- mapM hackageRulesFor hackagePackages
    hackageRule ~> need packageRules

hackagePackages :: [String]
hackagePackages = ["bytestring-0.10.8.1"]

hackageRulesFor :: String -> Rules String
hackageRulesFor package = do
    let rule = "hackage-" ++ package
    pd <- liftIO $ packageTmpDir package
    tarfile <- liftIO $ resolveFile pd $ package ++ ".tar.gz"
    tarfile $%> do
        ensureDir pd
        cmd
            (Cwd $ toFilePath $ parent tarfile)
            "wget"
            ("http://hackage.haskell.org/package/" ++ package ++ ".tar.gz")
            "--output-document"
            (toFilePath tarfile)
    rule ~> needP [tarfile]
    pure rule

hackageDir :: MonadIO m => m (Path Abs Dir)
hackageDir = (</> $(mkRelDir "hackage")) <$> tmpDir

packageTmpDir :: (MonadIO m, MonadThrow m) => String -> m (Path Abs Dir)
packageTmpDir package = do
    td <- hackageDir
    resolveDir td package
