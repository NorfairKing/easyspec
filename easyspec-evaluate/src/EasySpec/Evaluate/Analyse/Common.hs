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
tmpDir =
    (</> $(mkRelDir "tmp")) <$> buildDir

outDir :: MonadIO m => m (Path Abs Dir)
outDir = do
    (</> $(mkRelDir "out")) <$> buildDir

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

signatureInferenceStrategies :: [ES.SignatureInferenceStrategy]
signatureInferenceStrategies = ES.inferenceStrategiesToEvaluate
