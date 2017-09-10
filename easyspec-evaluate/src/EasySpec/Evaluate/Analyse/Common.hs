{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Common where

import Import

import Data.FileEmbed
import Language.Haskell.TH
import System.FilePath (dropExtensions)

import qualified EasySpec.Discover.SignatureInference as ES
import qualified EasySpec.Discover.Types as ES

buildDir :: MonadIO m => m (Path Abs Dir)
buildDir =
    liftIO $
    parseAbsDir
        $(makeRelativeToProject "." >>=
          (\d -> toFilePath <$> runIO (resolveDir' d)) >>=
          strToExp)

examplesDir :: MonadIO m => m (Path Abs Dir)
examplesDir = do
    bd <- buildDir
    liftIO $ resolveDir bd "../examples"

tmpDir :: MonadIO m => m (Path Abs Dir)
tmpDir = do
    bd <- buildDir
    liftIO $ resolveDir bd "tmp"

outDir :: MonadIO m => m (Path Abs Dir)
outDir = do
    bd <- buildDir
    liftIO $ resolveDir bd "out"

fileInDirWithExtensionAndComponents ::
       MonadIO m
    => m (Path Abs Dir)
    -> String
    -> Path Abs File
    -> [String]
    -> m (Path Abs File)
fileInDirWithExtensionAndComponents genDir ext f comps = do
    dd <- genDir
    let fileStr = intercalate "-" $ dropExtensions (toFilePath $ filename f) : comps
    liftIO $ (dd </>) <$> parseRelFile (concat [fileStr, ".", ext])

signatureInferenceStrategies :: [ES.SignatureInferenceStrategy]
signatureInferenceStrategies = ES.inferenceStrategiesToEvaluate
