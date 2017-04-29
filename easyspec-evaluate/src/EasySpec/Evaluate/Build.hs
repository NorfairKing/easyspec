{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Build where

import Import

import Data.Csv
import System.Environment
import System.FilePath (dropExtensions)

import qualified Data.ByteString.Lazy as LB

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Utils

import Development.Shake
import Development.Shake.Path

runBuild :: String -> IO ()
runBuild target =
    withArgs ["--color", target] $
    shakeArgs shakeOptions {shakeVerbosity = Loud} shakeBuild

shakeBuild :: Rules ()
shakeBuild = do
    dataFs <- dataRules
    analyseRule ~> needP dataFs

analyseRule :: String
analyseRule = "analyse"

examplesDir :: MonadIO m => m (Path Abs Dir)
examplesDir = liftIO $ resolveDir' "examples"

absSourceFile :: MonadIO m => Path Rel File -> m (Path Abs File)
absSourceFile f = (</> f) <$> examplesDir

tmpDir :: MonadIO m => m (Path Abs Dir)
tmpDir = liftIO $ resolveDir' "tmp"

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

dataFileFor ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor f name strat = do
    dd <- dataDir
    let fileStr =
            intercalate
                "-"
                [ dropExtensions (toFilePath f)
                , prettyPrint name
                , ES.sigInfStratName strat
                ]
    liftIO $ (dd </>) <$> parseRelFile (fileStr ++ ".csv")

outDir :: MonadIO m => m (Path Abs Dir)
outDir = liftIO $ resolveDir' "out"

dataRules :: Rules [Path Abs File]
dataRules = do
    edir <- examplesDir
    concat <$> forSourcesIn edir dataRulesForFile

dataRulesForFile :: Path Rel File -> Rules [Path Abs File]
dataRulesForFile sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    fss <-
        forM names $ \name ->
            forM signatureInferenceStrategies $ \infStrat -> do
                csvF <- dataFileFor sourceF name infStrat
                csvF $%> do
                    needP [absSourceF]
                    putLoud $
                        unwords
                            [ "Building data file"
                            , toFilePath csvF
                            , "by running 'easyspec-evaluate' on"
                            , toFilePath absSourceF
                            , "with focus:"
                            , prettyPrint name
                            , "and signature inference strategy:"
                            , ES.sigInfStratName infStrat
                            ]
                    liftIO $ do
                        ip <- getEvaluationInputPoint absSourceF name infStrat
                        ensureDir $ parent csvF
                        LB.writeFile (toFilePath csvF) $
                            encodeDefaultOrderedByName $
                            evaluationInputPointCsvLines ip
                pure csvF
    pure $ concat fss
