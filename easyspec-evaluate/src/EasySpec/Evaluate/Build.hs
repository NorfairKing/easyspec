{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Build where

import Import

import Data.Csv
import System.Environment
import System.Exit
import System.FilePath (dropExtensions)

import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as LB

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Types
import EasySpec.Utils

import Development.Shake
import Development.Shake.Path

runBuild :: String -> IO ()
runBuild target = do
    here <- getCurrentDir
    fs <- snd <$> listDir here
    case find ((== $(mkRelFile "easyspec-evaluate.cabal")) . filename) fs of
        Nothing ->
            die "easyspec-evaluate build is being run in the wrong directory."
        Just _ ->
            withArgs ["--color", target] $
            shakeArgs shakeOptions {shakeVerbosity = Loud} shakeBuild

shakeBuild :: Rules ()
shakeBuild = do
    dataF <- dataRules
    plotsFs <- plotsRules
    analyseRule ~> needP (dataF : plotsFs)

analyseRule :: String
analyseRule = "analyse"

examplesDir :: MonadIO m => m (Path Abs Dir)
examplesDir = liftIO $ resolveDir' "../examples"

absSourceFile :: MonadIO m => Path Rel File -> m (Path Abs File)
absSourceFile f = (</> f) <$> examplesDir

tmpDir :: MonadIO m => m (Path Abs Dir)
tmpDir = liftIO $ resolveDir' "tmp"

dataDir :: MonadIO m => m (Path Abs Dir)
dataDir = (</> $(mkRelDir "data")) <$> tmpDir

plotsDir :: MonadIO m => m (Path Abs Dir)
plotsDir = (</> $(mkRelDir "plots")) <$> tmpDir

dataFileFor ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
dataFileFor f name strat =
    csvDataFileWithComponents f [prettyPrint name, ES.sigInfStratName strat]

dataFileForExampleAndName ::
       MonadIO m => Path Rel File -> ES.EasyName -> m (Path Abs File)
dataFileForExampleAndName f name =
    csvDataFileWithComponents f [prettyPrint name]

csvDataFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvDataFileWithComponents f comps = do
    dd <- dataDir
    let fileStr = intercalate "-" $ dropExtensions (toFilePath f) : comps
    liftIO $ (dd </>) <$> parseRelFile (fileStr ++ ".csv")

dataFilesForExampleAndName ::
       MonadIO m => Path Rel File -> ES.EasyName -> m [Path Abs File]
dataFilesForExampleAndName file name =
    forM signatureInferenceStrategies $ dataFileFor file name

dataFileForExample :: MonadIO m => Path Rel File -> m (Path Abs File)
dataFileForExample f = csvDataFileWithComponents f []

dataFilesForExample :: MonadIO m => Path Rel File -> m [Path Abs File]
dataFilesForExample file = do
    absSourceF <- absSourceFile file
    names <- namesInSource absSourceF
    fmap concat $ forM names $ dataFilesForExampleAndName file

allDataFile :: MonadIO m => m (Path Abs File)
allDataFile = (</> $(mkRelFile "all.csv")) <$> dataDir

outDir :: MonadIO m => m (Path Abs Dir)
outDir = liftIO $ resolveDir' "out"

dataRules :: Rules (Path Abs File)
dataRules = do
    edir <- examplesDir
    csvFs <- forSourcesIn edir dataRulesForFile
    combF <- allDataFile
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

dataRulesForFile :: Path Rel File -> Rules (Path Abs File)
dataRulesForFile sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    csvFs <- forM names $ rulesForFileAndName sourceF
    combF <- dataFileForExample sourceF
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileAndName :: Path Rel File -> ES.EasyName -> Rules (Path Abs File)
rulesForFileAndName sourceF name = do
    csvFs <-
        forM signatureInferenceStrategies $
        rulesForFileNameAndStrat sourceF name
    combF <- dataFileForExampleAndName sourceF name
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileNameAndStrat ::
       Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
rulesForFileNameAndStrat sourceF name infStrat = do
    absSourceF <- absSourceFile sourceF
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
                encodeDefaultOrderedByName $ evaluationInputPointCsvLines ip
    pure csvF

combineCSVFiles ::
       forall a. (FromNamedRecord a, ToNamedRecord a, DefaultOrdered a)
    => Path Abs File
    -> [Path Abs File]
    -> Rules ()
combineCSVFiles res ins =
    res $%> do
        needP ins
        putLoud $
            unlines
                (unwords
                     ["Combining the following CSV files into", toFilePath res] :
                 map toFilePath ins)
        ls <- concat <$> mapM (readCSV @a) ins
        liftIO $ LB.writeFile (toFilePath res) $ encodeDefaultOrderedByName ls

readCSV :: (FromNamedRecord a, MonadIO m) => Path Abs File -> m [a]
readCSV file = do
    contents <- liftIO $ LB.readFile $ toFilePath file
    case decodeByName contents of
        Left err ->
            fail $
            unwords
                ["Failed to read CSV file", toFilePath file, "with error:", err]
        Right (_, res) -> pure $ V.toList res

dataFrom ::
       Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Action [EvaluatorCsvLine]
dataFrom file name strat = do
    dataFile <- dataFileFor file name strat
    needP [dataFile]
    readCSV dataFile

dataFromExampleAndName ::
       Path Rel File -> ES.EasyName -> Action [EvaluatorCsvLine]
dataFromExampleAndName file name = do
    dataFile <- dataFileForExampleAndName file name
    needP [dataFile]
    readCSV dataFile

dataFromExample :: Path Rel File -> Action [EvaluatorCsvLine]
dataFromExample file = do
    dataFile <- dataFileForExample file
    needP [dataFile]
    readCSV dataFile

dataFromAll :: Action [EvaluatorCsvLine]
dataFromAll = do
    dataFile <- allDataFile
    needP [dataFile]
    readCSV dataFile

plotsRules :: Rules [Path Abs File]
plotsRules = pure []
