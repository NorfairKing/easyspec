{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Evaluate.Analyse.Data.Averages where

import Import

import Data.Aeson as JSON
import Data.Csv as CSV hiding ((.=), (.:))

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Content

averageDataRule :: String
averageDataRule = "average-data"

averageDataRules :: Rules ()
averageDataRules = do
    exs <- examples
    fs <- concat <$> mapM averageOverNamesPerStrategyForExampleRules exs
    fss <- mapM averageOverNamesAndStrategiesForExampleRules exs
    averageDataRule ~> needP (fs ++ fss)

averageOverNamesPerStrategyForExampleRules ::
       ES.InputSpec -> Rules [Path Abs File]
averageOverNamesPerStrategyForExampleRules is = do
    jf <- averageOverNamesPerStrategyForExampleJSONRules is
    cf <- averageOverNamesPerStrategyForExampleCSVRules is
    pure [jf, cf]

averageOverNamesPerStrategyForExampleJSONRules ::
       ES.InputSpec -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleJSONRules is = do
    avgF <- averageOverNamesPerStrategyForExampleJSONFile is
    avgF $%> do
        dataPoints <- dataFromExample is
        let averages =
                flip
                    map
                    (averagePer (eclEvaluatorName &&& eclStratName) dataPoints) $ \((n, s), a) ->
                    ( s
                    , AverageEvaluatorOutput
                      { averageEvaluatorOutputEvaluatorName = n
                      , averageEvaluatorOutputAverage = a
                      })
        let averagesPerStrategies =
                flip map (groupUnorderedBy fst averages) $ \(s, ls) ->
                    AverageEvaluatorPerStrategyOutput
                    { averageEvaluatorPerStrategyStrategy = s
                    , averageEvaluatorPerStrategyAverageEvaluatorOutput =
                          map snd ls
                    }
        let avoverNamesPerStrat =
                AverageOverNamesForExampleAndStrategy
                { averageOverNamesForExampleAndStrategyExample = is
                , averageOverNamesForExampleAndStrategyAverage =
                      averagesPerStrategies
                }
        writeJSON avgF avoverNamesPerStrat
    pure avgF

averageOverNamesPerStrategyForExampleCSVRules ::
       ES.InputSpec -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleCSVRules is = do
    avgcsvF <- averageOverNamesPerStrategyForExampleCSVFile is
    avgcsvF $%> do
        sf <- averageOverNamesPerStrategyForExampleJSONFile is
        needP [sf]
        averageOverNamesForExampleAndStrategy <- readJSON sf
        let ls =
                makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy
                    averageOverNamesForExampleAndStrategy
        writeCSV avgcsvF ls
    pure avgcsvF

averageOverNamesPerStrategyForExampleJSONFile ::
       MonadIO m => ES.InputSpec -> m (Path Abs File)
averageOverNamesPerStrategyForExampleJSONFile is =
    jsonAverageFileWithComponents (ES.inputSpecFile is) ["average-per-strategy"]

averageOverNamesPerStrategyForExampleCSVFile ::
       MonadIO m => ES.InputSpec -> m (Path Abs File)
averageOverNamesPerStrategyForExampleCSVFile is =
    csvAverageFileWithComponents (ES.inputSpecFile is) ["average-per-strategy"]

data AverageOverNamesForExampleAndStrategy = AverageOverNamesForExampleAndStrategy
    { averageOverNamesForExampleAndStrategyExample :: ES.InputSpec
    , averageOverNamesForExampleAndStrategyAverage :: [AverageEvaluatorPerStrategyOutput]
    } deriving (Show, Eq, Generic)

instance ToJSON AverageOverNamesForExampleAndStrategy where
    toJSON AverageOverNamesForExampleAndStrategy {..} =
        object
            [ "example" .= averageOverNamesForExampleAndStrategyExample
            , "averages" .= averageOverNamesForExampleAndStrategyAverage
            ]

instance FromJSON AverageOverNamesForExampleAndStrategy where
    parseJSON =
        withObject "AverageOverNamesForExampleAndStrategy" $ \o ->
            AverageOverNamesForExampleAndStrategy <$> o .: "example" <*>
            o .: "averages"

data AverageEvaluatorPerStrategyOutput = AverageEvaluatorPerStrategyOutput
    { averageEvaluatorPerStrategyStrategy :: String
    , averageEvaluatorPerStrategyAverageEvaluatorOutput :: [AverageEvaluatorOutput]
    } deriving (Show, Eq, Generic)

instance ToJSON AverageEvaluatorPerStrategyOutput where
    toJSON AverageEvaluatorPerStrategyOutput {..} =
        object
            [ "strategy" .= averageEvaluatorPerStrategyStrategy
            , "average" .= averageEvaluatorPerStrategyAverageEvaluatorOutput
            ]

instance FromJSON AverageEvaluatorPerStrategyOutput where
    parseJSON =
        withObject "AverageEvaluatorPerStrategyOutput" $ \o ->
            AverageEvaluatorPerStrategyOutput <$> o .: "strategy" <*>
            o .: "average"

averageOverNamesAndStrategiesForExampleRules ::
       ES.InputSpec -> Rules (Path Abs File)
averageOverNamesAndStrategiesForExampleRules is = do
    avgF <- averageOverNamesAndStrategiesForExampleFile is
    avgF $%> do
        dataPoints <- dataFromExample is
        let averages =
                map
                    (\(n, a) ->
                         AverageEvaluatorOutput
                         { averageEvaluatorOutputEvaluatorName = n
                         , averageEvaluatorOutputAverage = a
                         }) $
                averagePer eclEvaluatorName dataPoints
        let avoverNames =
                AverageOverNamesAndStrategiesForExample
                { averageOverNamesAndStrategiesForExampleExample = is
                , averageOverNamesAndStrategiesForExampleAverage = averages
                }
        writeJSON avgF avoverNames
    pure avgF

averagePer ::
       Eq a
    => (EvaluatorCsvLine -> a)
    -> [EvaluatorCsvLine]
    -> [(a, AverageOutput)]
averagePer func ls =
    map (second averageEvaluatorCsvLines) $ groupUnorderedBy func ls

groupUnorderedBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupUnorderedBy func ls =
    let oups = nub $ map func ls
    in flip map oups $ \oup -> (oup, filter ((== oup) . func) ls)

averageOverNamesAndStrategiesForExampleFile ::
       MonadIO m => ES.InputSpec -> m (Path Abs File)
averageOverNamesAndStrategiesForExampleFile is =
    jsonAverageFileWithComponents (ES.inputSpecFile is) ["average"]

data AverageOverNamesAndStrategiesForExample = AverageOverNamesAndStrategiesForExample
    { averageOverNamesAndStrategiesForExampleExample :: ES.InputSpec
    , averageOverNamesAndStrategiesForExampleAverage :: [AverageEvaluatorOutput]
    } deriving (Show, Eq, Generic)

instance ToJSON ES.InputSpec where
    toJSON ES.InputSpec {..} =
        object ["base dir" .= inputSpecBaseDir, "file" .= inputSpecFile]

instance FromJSON ES.InputSpec where
    parseJSON =
        withObject "InputSpec" $ \o ->
            ES.InputSpec <$> o .: "base dir" <*> o .: "file"

instance ToJSON AverageOverNamesAndStrategiesForExample where
    toJSON AverageOverNamesAndStrategiesForExample {..} =
        object
            [ "example" .= averageOverNamesAndStrategiesForExampleExample
            , "averages" .= averageOverNamesAndStrategiesForExampleAverage
            ]

instance FromJSON AverageOverNamesAndStrategiesForExample where
    parseJSON =
        withObject "AverageOverNamesAndStrategiesForExample" $ \o ->
            AverageOverNamesAndStrategiesForExample <$> o .: "example" <*>
            o .: "averages"

data AverageEvaluatorOutput = AverageEvaluatorOutput
    { averageEvaluatorOutputEvaluatorName :: String
    , averageEvaluatorOutputAverage :: AverageOutput
    } deriving (Show, Eq, Generic)

instance ToJSON AverageEvaluatorOutput where
    toJSON AverageEvaluatorOutput {..} =
        object
            [ "evaluator" .= averageEvaluatorOutputEvaluatorName
            , "average" .= averageEvaluatorOutputAverage
            ]

instance FromJSON AverageEvaluatorOutput where
    parseJSON =
        withObject "AverageEvaluatorOutput" $ \o ->
            AverageEvaluatorOutput <$> o .: "evaluator" <*> o .: "average"

data AverageCsvLine = AverageCsvLine
    { averageCsvLineBaseDir :: Path Abs Dir
    , averageCsvLineSourceFile :: Path Rel File
    , averageCsvLineEvaluatorName :: String
    , averageCsvLineStrategyName :: String
    , averageCsvLineAverage :: AverageOutput
    } deriving (Show, Eq, Generic)

instance ToNamedRecord AverageCsvLine where
    toNamedRecord AverageCsvLine {..} =
        namedRecord
            [ ("base dir", toField $ toFilePath averageCsvLineBaseDir)
            , ("source", toField $ toFilePath averageCsvLineSourceFile)
            , ("evaluator", toField averageCsvLineEvaluatorName)
            , ("strategy", toField averageCsvLineStrategyName)
            , ("avg", toField $ averageOutputAverage averageCsvLineAverage)
            , ("stddev", toField $ averageOutputStdDev averageCsvLineAverage)
            ]

instance DefaultOrdered AverageCsvLine where
    headerOrder _ =
        header ["base dir", "source", "evaluator", "strategy", "avg", "stddev"]

makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy ::
       AverageOverNamesForExampleAndStrategy -> [AverageCsvLine]
makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy AverageOverNamesForExampleAndStrategy {..} =
    concatMap
        (makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput
             averageOverNamesForExampleAndStrategyExample)
        averageOverNamesForExampleAndStrategyAverage

makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput ::
       ES.InputSpec -> AverageEvaluatorPerStrategyOutput -> [AverageCsvLine]
makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput is AverageEvaluatorPerStrategyOutput {..} =
    map
        (makeAverageCsvLinesFromAverageEvaluatorOutput
             is
             averageEvaluatorPerStrategyStrategy)
        averageEvaluatorPerStrategyAverageEvaluatorOutput

makeAverageCsvLinesFromAverageOverNamesAndStrategiesForExample ::
       String -> AverageOverNamesAndStrategiesForExample -> [AverageCsvLine]
makeAverageCsvLinesFromAverageOverNamesAndStrategiesForExample stratName AverageOverNamesAndStrategiesForExample {..} =
    map
        (makeAverageCsvLinesFromAverageEvaluatorOutput
             averageOverNamesAndStrategiesForExampleExample
             stratName)
        averageOverNamesAndStrategiesForExampleAverage

makeAverageCsvLinesFromAverageEvaluatorOutput ::
       ES.InputSpec -> String -> AverageEvaluatorOutput -> AverageCsvLine
makeAverageCsvLinesFromAverageEvaluatorOutput is stratName AverageEvaluatorOutput {..} =
    AverageCsvLine
    { averageCsvLineBaseDir = ES.inputSpecBaseDir is
    , averageCsvLineSourceFile = ES.inputSpecFile is
    , averageCsvLineEvaluatorName = averageEvaluatorOutputEvaluatorName
    , averageCsvLineStrategyName = stratName
    , averageCsvLineAverage = averageEvaluatorOutputAverage
    }

jsonAverageFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
jsonAverageFileWithComponents = averagesFile "json"

csvAverageFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
csvAverageFileWithComponents = averagesFile "csv"

averagesFile ::
       MonadIO m => String -> Path Rel File -> [String] -> m (Path Abs File)
averagesFile = fileInDirWithExtensionAndComponents averagesDir

averagesDir :: MonadIO m => m (Path Abs Dir)
averagesDir = (</> $(mkRelDir "averages")) <$> tmpDir

averageEvaluatorCsvLines :: [EvaluatorCsvLine] -> AverageOutput
averageEvaluatorCsvLines ecsvls =
    let a = averageOrZero $ map eclEvaluatorOutput ecsvls
    in AverageOutput
       { averageOutputAverage = a
       , averageOutputStdDev =
             sqrt $
             averageOrZero $
             map (\e -> (a - eclEvaluatorOutput e) ^ (2 :: Int)) ecsvls
       }

data AverageOutput = AverageOutput
    { averageOutputAverage :: Double
    , averageOutputStdDev :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON AverageOutput where
    toJSON AverageOutput {..} =
        object
            [ "average" .= averageOutputAverage
            , "standard-deviation" .= averageOutputStdDev
            ]

instance FromJSON AverageOutput where
    parseJSON =
        withObject "AverageOutput" $ \o ->
            AverageOutput <$> o .: "average" <*> o .: "standard-deviation"

averageOrZero :: [Double] -> Double
averageOrZero [] = 0
averageOrZero ls = sum ls / genericLength ls