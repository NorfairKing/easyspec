{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Data.Averages where

import Import

import Data.Aeson as JSON
import Data.Csv as CSV hiding ((.=), (.:))

import Development.Shake
import Development.Shake.Path

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
       Path Rel File -> Rules [Path Abs File]
averageOverNamesPerStrategyForExampleRules sourceF = do
    jf <- averageOverNamesPerStrategyForExampleJSONRules sourceF
    cf <- averageOverNamesPerStrategyForExampleCSVRules sourceF
    pure [jf, cf]

averageOverNamesPerStrategyForExampleJSONRules ::
       Path Rel File -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleJSONRules sourceF = do
    avgF <- averageOverNamesPerStrategyForExampleJSONFile sourceF
    avgF $%> do
        dataPoints <- dataFromExample sourceF
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
                { averageOverNamesForExampleAndStrategyExample = sourceF
                , averageOverNamesForExampleAndStrategyAverage =
                      averagesPerStrategies
                }
        writeJSON avgF avoverNamesPerStrat
    pure avgF

averageOverNamesPerStrategyForExampleCSVRules ::
       Path Rel File -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleCSVRules sourceF = do
    avgcsvF <- averageOverNamesPerStrategyForExampleCSVFile sourceF
    avgcsvF $%> do
        sf <- averageOverNamesPerStrategyForExampleJSONFile sourceF
        needP [sf]
        averageOverNamesForExampleAndStrategy <- readJSON sf
        let ls =
                makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy
                    averageOverNamesForExampleAndStrategy
        writeCSV avgcsvF ls
    pure avgcsvF

averageOverNamesPerStrategyForExampleJSONFile ::
       MonadIO m => Path Rel File -> m (Path Abs File)
averageOverNamesPerStrategyForExampleJSONFile sourceF =
    jsonAverageFileWithComponents sourceF ["average-per-strategy"]

averageOverNamesPerStrategyForExampleCSVFile ::
       MonadIO m => Path Rel File -> m (Path Abs File)
averageOverNamesPerStrategyForExampleCSVFile sourceF =
    csvAverageFileWithComponents sourceF ["average-per-strategy"]

data AverageOverNamesForExampleAndStrategy = AverageOverNamesForExampleAndStrategy
    { averageOverNamesForExampleAndStrategyExample :: Path Rel File
    , averageOverNamesForExampleAndStrategyAverage :: [AverageEvaluatorPerStrategyOutput]
    } deriving (Show, Eq, Generic)

instance ToJSON AverageOverNamesForExampleAndStrategy where
    toJSON AverageOverNamesForExampleAndStrategy {..} =
        object
            [ "example" .=
              toFilePath averageOverNamesForExampleAndStrategyExample
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
       Path Rel File -> Rules (Path Abs File)
averageOverNamesAndStrategiesForExampleRules sourceF = do
    avgF <- averageOverNamesAndStrategiesForExampleFile sourceF
    avgF $%> do
        dataPoints <- dataFromExample sourceF
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
                { averageOverNamesAndStrategiesForExampleExample = sourceF
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
       MonadIO m => Path Rel File -> m (Path Abs File)
averageOverNamesAndStrategiesForExampleFile sourceF =
    jsonAverageFileWithComponents sourceF ["average"]

data AverageOverNamesAndStrategiesForExample = AverageOverNamesAndStrategiesForExample
    { averageOverNamesAndStrategiesForExampleExample :: Path Rel File
    , averageOverNamesAndStrategiesForExampleAverage :: [AverageEvaluatorOutput]
    } deriving (Show, Eq, Generic)

instance ToJSON AverageOverNamesAndStrategiesForExample where
    toJSON AverageOverNamesAndStrategiesForExample {..} =
        object
            [ "example" .=
              toFilePath averageOverNamesAndStrategiesForExampleExample
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
    { averageCsvLineSourceFile :: Path Rel File
    , averageCsvLineEvaluatorName :: String
    , averageCsvLineStrategyName :: String
    , averageCsvLineAverage :: AverageOutput
    } deriving (Show, Eq, Generic)

instance ToNamedRecord AverageCsvLine where
    toNamedRecord AverageCsvLine {..} =
        namedRecord
            [ ("source", toField $ toFilePath averageCsvLineSourceFile)
            , ("evaluator", toField averageCsvLineEvaluatorName)
            , ("strategy", toField averageCsvLineStrategyName)
            , ("avg", toField $ averageOutputAverage averageCsvLineAverage)
            , ("stddev", toField $ averageOutputStdDev averageCsvLineAverage)
            ]

instance DefaultOrdered AverageCsvLine where
    headerOrder _ = header ["source", "evaluator", "strategy", "avg", "stddev"]

makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy ::
       AverageOverNamesForExampleAndStrategy -> [AverageCsvLine]
makeAverageCsvLinesFromAverageOverNamesForExampleAndStrategy AverageOverNamesForExampleAndStrategy {..} =
    concatMap
        (makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput
             averageOverNamesForExampleAndStrategyExample)
        averageOverNamesForExampleAndStrategyAverage

makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput ::
       Path Rel File -> AverageEvaluatorPerStrategyOutput -> [AverageCsvLine]
makeAverageCsvLinesFromAverageEvaluatorPerStrategyOutput sourceF AverageEvaluatorPerStrategyOutput {..} =
    map
        (makeAverageCsvLinesFromAverageEvaluatorOutput
             sourceF
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
       Path Rel File -> String -> AverageEvaluatorOutput -> AverageCsvLine
makeAverageCsvLinesFromAverageEvaluatorOutput sourceF stratName AverageEvaluatorOutput {..} =
    AverageCsvLine
    { averageCsvLineSourceFile = sourceF
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
