{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Data.Averages where

import Import

import Data.Aeson as JSON
import Data.Csv as CSV hiding ((.:), (.=))

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Discover.Utils as ES

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Content
import EasySpec.Evaluate.Analyse.Data.Files

averageDataRule :: String
averageDataRule = "average-data"

averageDataRules :: Rules ()
averageDataRules = do
    fs <-
        concat <$>
        mapM
            (uncurry averageOverNamesPerStrategyForExampleRules)
            groupsAndExamples
    fss <-
        mapM
            (uncurry averageOverNamesAndStrategiesForExampleRules)
            groupsAndExamples
    averageDataRule ~> needP (fs ++ fss)

averageOverNamesPerStrategyForExampleRules ::
       GroupName -> Example -> Rules [Path Abs File]
averageOverNamesPerStrategyForExampleRules groupName is = do
    jf <- averageOverNamesPerStrategyForExampleJSONRules groupName is
    cf <- averageOverNamesPerStrategyForExampleCSVRules groupName is
    pure [jf, cf]

averageOverNamesPerStrategyForExampleJSONRules ::
       GroupName -> Example -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleJSONRules groupName is = do
    avgF <- averageOverNamesPerStrategyForExampleJSONFile is
    avgF $%> do
        dataPoints <- dataFromExample groupName is
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
       GroupName -> Example -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleCSVRules _ is = do
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
       GroupName -> Example -> Rules (Path Abs File)
averageOverNamesAndStrategiesForExampleRules groupName is = do
    avgF <- averageOverNamesAndStrategiesForExampleFile is
    avgF $%> do
        dataPoints <- dataFromExample groupName is
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
       Ord a
    => (EvaluatorCsvLine -> a)
    -> [EvaluatorCsvLine]
    -> [(a, AverageOutput)]
averagePer func ls =
    map (second averageEvaluatorCsvLines) $ groupUnorderedBy func ls

groupUnorderedBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupUnorderedBy func ls =
    let oups = ES.ordNub $ map func ls
    in flip map oups $ \oup -> (oup, filter ((== oup) . func) ls)

averageOverNamesAndStrategiesForExampleFile ::
       MonadIO m => ES.InputSpec -> m (Path Abs File)
averageOverNamesAndStrategiesForExampleFile is =
    jsonAverageFileWithComponents (ES.inputSpecFile is) ["average"]

data AverageOverNamesAndStrategiesForExample = AverageOverNamesAndStrategiesForExample
    { averageOverNamesAndStrategiesForExampleExample :: ES.InputSpec
    , averageOverNamesAndStrategiesForExampleAverage :: [AverageEvaluatorOutput]
    } deriving (Show, Eq, Generic)

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
    , averageCsvLineSourceFile :: Path Abs File
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
    map (makeAverageCsvLinesFromAverageEvaluatorOutput
             is
             averageEvaluatorPerStrategyStrategy)
        averageEvaluatorPerStrategyAverageEvaluatorOutput

makeAverageCsvLinesFromAverageOverNamesAndStrategiesForExample ::
       String -> AverageOverNamesAndStrategiesForExample -> [AverageCsvLine]
makeAverageCsvLinesFromAverageOverNamesAndStrategiesForExample stratName AverageOverNamesAndStrategiesForExample {..} =
    map (makeAverageCsvLinesFromAverageEvaluatorOutput
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
       MonadIO m => Path Abs File -> [String] -> m (Path Abs File)
jsonAverageFileWithComponents = averagesFile "json"

csvAverageFileWithComponents ::
       MonadIO m => Path Abs File -> [String] -> m (Path Abs File)
csvAverageFileWithComponents = averagesFile "csv"

averagesFile ::
       MonadIO m => String -> Path Abs File -> [String] -> m (Path Abs File)
averagesFile = fileInDirWithExtensionAndComponents averagesDir

averagesDir :: MonadIO m => m (Path Abs Dir)
averagesDir = (</> $(mkRelDir "averages")) <$> dataDir

averageEvaluatorCsvLines :: [EvaluatorCsvLine] -> AverageOutput
averageEvaluatorCsvLines ecsvls =
    let avg = average $ mapMaybe eclEvaluatorOutput ecsvls
    in AverageOutput
       { averageOutputAverage = avg
       , averageOutputStdDev =
             do a <- avg
                sqrt <$>
                    average
                        (map (\e -> (a - e) ^ (2 :: Int)) $
                         mapMaybe eclEvaluatorOutput ecsvls)
       }

data AverageOutput = AverageOutput
    { averageOutputAverage :: Maybe Double
    , averageOutputStdDev :: Maybe Double
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

average :: [Double] -> Maybe Double
average [] = Nothing
average ls = Just $ sum ls / genericLength ls
