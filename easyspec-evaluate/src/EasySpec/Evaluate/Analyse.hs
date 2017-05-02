{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse where

import Import

import Data.Csv
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

analyseRules :: Rules ()
analyseRules = do
    dataF <- dataRules
    plotsFs <- plotsRules
    zf <- analysisZipFileRules
    analyseRule ~> do
        needP (dataF : plotsFs)
        needP [zf]

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
csvDataFileWithComponents = fileInDirWithExtensionAndComponents dataDir "csv"

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

forExamples :: MonadIO m => (Path Rel File -> m a) -> m [a]
forExamples func = do
    edir <- examplesDir
    forSourcesIn edir func

dataRules :: Rules (Path Abs File)
dataRules = do
    ghciResource <- newResource "ghci" 1
    csvFs <- forExamples (dataRulesForExample ghciResource)
    combF <- allDataFile
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

dataRulesForExample :: Resource -> Path Rel File -> Rules (Path Abs File)
dataRulesForExample ghciResource sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    csvFs <- forM names $ rulesForFileAndName ghciResource sourceF
    combF <- dataFileForExample sourceF
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileAndName ::
       Resource -> Path Rel File -> ES.EasyName -> Rules (Path Abs File)
rulesForFileAndName ghciResource sourceF name = do
    csvFs <-
        forM signatureInferenceStrategies $
        rulesForFileNameAndStrat ghciResource sourceF name
    combF <- dataFileForExampleAndName sourceF name
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileNameAndStrat ::
       Resource
    -> Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
rulesForFileNameAndStrat ghciResource sourceF name infStrat = do
    absSourceF <- absSourceFile sourceF
    csvF <- dataFileFor sourceF name infStrat
    csvF $%> do
        needP [absSourceF]
        ip <-
            withResource ghciResource 1 $ do
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
                liftIO $ getEvaluationInputPoint absSourceF name infStrat
        liftIO $ do
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

pngPlotFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
pngPlotFileWithComponents = fileInDirWithExtensionAndComponents plotsDir "png"

singleEvaluatorBarPlotFileForExampleAndName ::
       MonadIO m
    => Path Rel File
    -> ES.EasyName
    -> Evaluator a
    -> m (Path Abs File)
singleEvaluatorBarPlotFileForExampleAndName file name ev =
    pngPlotFileWithComponents
        file
        ["runtime", prettyPrint name, evaluatorName ev]

plotsRules :: Rules [Path Abs File]
plotsRules = concat <$> forExamples plotsRulesForExample

plotsRulesForExample :: Path Rel File -> Rules [Path Abs File]
plotsRulesForExample sourceF = do
    absSourceF <- absSourceFile sourceF
    names <- namesInSource absSourceF
    fmap concat $ forM names $ plotsRulesForExampleAndName sourceF

scriptFile :: MonadIO m => String -> m (Path Abs File)
scriptFile fname = liftIO $ resolveFile' $ "rscripts/" ++ fname

singleEvaluatorBarAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorBarAnalysisScript = scriptFile "single_evaluator_bar.r"

plotsRulesForExampleAndName ::
       Path Rel File -> ES.EasyName -> Rules [Path Abs File]
plotsRulesForExampleAndName sourceF name = do
    singleEvaluatorBarScript <- singleEvaluatorBarAnalysisScript
    dataFile <- dataFileForExampleAndName sourceF name
    forM evaluators $ \(AnyEvaluator evaluator) -> do
        runtimePlotFile <-
            singleEvaluatorBarPlotFileForExampleAndName sourceF name evaluator
        runtimePlotFile $%> do
            needP [singleEvaluatorBarScript, dataFile]
            cmd
                "Rscript"
                (toFilePath singleEvaluatorBarScript)
                (toFilePath dataFile)
                (toFilePath runtimePlotFile)
                (toFilePath sourceF)
                (prettyPrint name)
                (evaluatorName evaluator)
        pure runtimePlotFile

analysisZipFile :: MonadIO m => m (Path Abs File)
analysisZipFile = do
    od <- outDir
    liftIO $ resolveFile od "easyspec-data.tar.gz"

analysisZipFileRules :: Rules (Path Abs File)
analysisZipFileRules = do
    zf <- analysisZipFile
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
    pure zf
