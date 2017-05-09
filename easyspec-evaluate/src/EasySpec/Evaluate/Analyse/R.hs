{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.R
    ( rRules
    , rscript
    , needRLibs
    ) where

import Import

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Utils

rscript :: [String] -> Action ()
rscript args = do
    commonR <- commonRFile
    rBin <- rBinary
    needP [commonR, rBin]
    needRLibs ["ggplot2"]
    libDir <- rlibdir
    cmd
        (toFilePath rBin)
        (AddEnv "R_LIBS" $ toFilePath libDir)
        (toFilePath commonR)
        args

rRules :: Rules ()
rRules = do
    buildRRules
    rLibsRules

rTmpDir :: MonadIO m => m (Path Abs Dir)
rTmpDir = (</> $(mkRelDir "r")) <$> tmpDir

rArchive :: MonadIO m => m (Path Abs File)
rArchive = (</> $(mkRelFile "R.tar.gz")) <$> rTmpDir

rVersion :: String
rVersion = "R-3.2.0"

rLink :: FilePath
rLink = "https://cran.r-project.org/src/base/R-3/" ++ rVersion ++ ".tar.gz"

rDir :: MonadIO m => m (Path Abs Dir)
rDir = (</> $(mkRelDir "R")) <$> rTmpDir

rBinary :: MonadIO m => m (Path Abs File)
rBinary = (</> $(mkRelFile "Rscript")) <$> rTmpDir

rMakeDir :: MonadIO m => m (Path Abs Dir)
rMakeDir = do
    rVersionDir <- liftIO $ parseRelDir rVersion
    rd <- rDir
    pure $ rd </> rVersionDir

rConfigureScriptName :: Path Rel File
rConfigureScriptName = $(mkRelFile "configure")

rConfigureScript :: MonadIO m => m (Path Abs File)
rConfigureScript = (</> rConfigureScriptName) <$> rMakeDir

rMakefile :: MonadIO m => m (Path Abs File)
rMakefile = (</> $(mkRelFile "Makefile")) <$> rMakeDir

rBinInMakeDir :: MonadIO m => m (Path Abs File)
rBinInMakeDir =
    (</> ($(mkRelDir "bin") </> $(mkRelFile "Rscript"))) <$> rMakeDir

rInstallBinInMakeDir :: MonadIO m => m (Path Abs File)
rInstallBinInMakeDir =
    (</> ($(mkRelDir "bin") </> $(mkRelFile "R"))) <$> rMakeDir

rlibdir :: MonadIO m => m (Path Abs Dir)
rlibdir = (</> $(mkRelDir "library")) <$> rMakeDir

buildRRules :: Rules ()
buildRRules = do
    archFile <- rArchive
    archFile $%> cmd "curl" "--output" (toFilePath archFile) rLink
    confScriptFile <- rConfigureScript
    confScriptFile $%> do
        needP [archFile]
        rdir <- rDir
        cmd
            "tar"
            "--extract"
            "--verbose"
            "--file"
            (toFilePath archFile)
            "--directory"
            (toFilePath rdir)
    makeFile <- rMakefile
    makeDir <- rMakeDir
    makeFile $%> do
        needP [confScriptFile]
        cmd (Cwd $ toFilePath makeDir) ("./" ++ toFilePath rConfigureScriptName)
    binInMakeDir <- rBinInMakeDir
    installBinInMakeDir <- rInstallBinInMakeDir
    [binInMakeDir, installBinInMakeDir] $&%> do
        needP [makeFile]
        cmd (Cwd $ toFilePath makeDir) "make" "--jobs"
    bin <- rBinary
    bin `byCopying` binInMakeDir

rLibTarget :: MonadIO m => String -> m (Path Abs File)
rLibTarget name = do
    libDir <- rlibdir
    dir <- liftIO $ parseRelDir name
    file <- liftIO $ parseRelFile name
    pure $ libDir </> dir </> $(mkRelDir "R") </> file

needRLibs :: [String] -> Action ()
needRLibs names = do
    ts <- mapM rLibTarget names
    needP ts

rLibsRules :: Rules ()
rLibsRules = mapM_ rlib rLibs

rLibs :: [String]
rLibs = ["ggplot2"]

rlib :: String -> Rules ()
rlib name = do
    rBin <- rBinary
    targetFile <- rLibTarget name
    targetFile $%> do
        needP [rBin]
        tmp <- rTmpDir
        scriptFileName <- liftIO $ parseRelFile $ "install-" ++ name ++ ".r"
        let tmpInstallScript = tmp </> scriptFileName
        libdir <- rlibdir
        writeFile' (toFilePath tmpInstallScript) $
            unlines
                [ "repos <- \"http://cran.rstudio.com\""
                , "libloc <- \"" ++ toFilePath libdir ++ "\""
                , "update.packages(repos=repos, ask=FALSE, lib=libloc)"
                , "install.packages(c(\"" ++
                  name ++ "\"), repos=repos, lib=libloc)"
                ]
        cmd (toFilePath rBin) (toFilePath tmpInstallScript)
