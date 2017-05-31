{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Hackage where

import Import

import Path.IO

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Hackage.TH

hackageRule :: String
hackageRule = "hackage"

hackageRules :: Resource -> Rules ()
hackageRules ghciResource = do
    packageRules <- mapM (hackageRulesFor ghciResource) hackagePackages
    hackageRule ~> need packageRules

hackagePackages :: [(PackageName, [String], [String])]
hackagePackages =
    [ $(makePackageTup "bytestring-0.10.8.1" ["Data.ByteString"])
    , $(makePackageTup "pretty-1.1.3.5" ["Text.PrettyPrint"])
    ]

hackageRulesFor :: Resource -> (PackageName, [String], [String]) -> Rules String
hackageRulesFor ghciResource (package, sourceDirs, modulePaths) = do
    tarfile <- tarfileRules package
    let rule = "hackage-" ++ package
    rule ~> needP [tarfile]
    pure rule

tarfileRules :: PackageName -> Rules (Path Abs File)
tarfileRules package = do
    pd <- liftIO $ packageTmpDir package
    tarfile <- liftIO $ resolveFile pd $ package ++ ".tar.gz"
    tarfile $%> do
        ensureDir pd
        unit $
            cmd
                (Cwd $ toFilePath $ parent tarfile)
                "wget"
                ("http://hackage.haskell.org/package/" ++ package ++ ".tar.gz")
                "--output-document"
                (toFilePath tarfile)
        unit $
            cmd
                (Cwd $ toFilePath $ parent tarfile)
                "tar"
                "xvzf"
                (toFilePath tarfile)
    pure tarfile

hackageDir
    :: MonadIO m
    => m (Path Abs Dir)
hackageDir = (</> $(mkRelDir "hackage")) <$> tmpDir

packageExamples
    :: MonadIO m
    => (PackageName, [String], [String]) -> m [ES.InputSpec]
packageExamples (package, sourceDirs, modulePaths) = do
    pd <- packageDir package
    let modulesIn sourceDir =
            fmap catMaybes $
            forM modulePaths $ \modulePath -> do
                bd <- liftIO $ resolveDir pd sourceDir
                fp <- liftIO $ parseRelFile $ modulePath ++ ".hs"
                exists <- liftIO $ Path.IO.doesFileExist $ bd </> fp
                pure $
                    if exists
                        then Just
                                 ES.InputSpec
                                 { ES.inputSpecBaseDir = bd
                                 , ES.inputSpecFile = fp
                                 }
                        else Nothing
    concat <$> forM sourceDirs modulesIn
