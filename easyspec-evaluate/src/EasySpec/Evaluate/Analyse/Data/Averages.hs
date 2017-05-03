{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate.Analyse.Data.Averages where

import Import

import Data.Aeson as JSON

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
    fs <- mapM averageOverNamesAndStrategiesForExampleRules exs
    fss <- mapM averageOverNamesPerStrategyForExampleRules exs
    averageDataRule ~> needP (fs ++ fss)

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
    let oups = nub $ map func ls
    in flip map oups $ \oup ->
           (oup, averageEvaluatorCsvLines $ filter ((== oup) . func) ls)

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

data AverageEvaluatorOutput = AverageEvaluatorOutput
    { averageEvaluatorOutputEvaluatorName :: String
    , averageEvaluatorOutputAverage :: AverageOutput
    } deriving (Show, Eq, Generic)

instance ToJSON AverageEvaluatorOutput where
    toJSON AverageEvaluatorOutput {..} =
        object
            [ "name" .= averageEvaluatorOutputEvaluatorName
            , "average" .= averageEvaluatorOutputAverage
            ]

averageOverNamesPerStrategyForExampleRules ::
       Path Rel File -> Rules (Path Abs File)
averageOverNamesPerStrategyForExampleRules sourceF = do
    avgF <- averageOverNamesPerStrategyForExampleFile sourceF
    avgF $%> do
        dataPoints <- dataFromExample sourceF
        let averages =
                map
                    (\((n, s), a) ->
                         AverageEvaluatorPerStrategyOutput
                         { averageEvaluatorPerStrategyEvaluatorName = n
                         , averageEvaluatorPerStrategyStrategy = s
                         , averageEvaluatorPerStrategyAverage = a
                         }) $
                averagePer (eclEvaluatorName &&& eclStratName) dataPoints
        let avoverNamesPerStrat =
                AverageOverNamesForExampleAndStrategy
                { averageOverNamesForExampleAndStrategyExample = sourceF
                , averageOverNamesForExampleAndStrategyAverage = averages
                }
        writeJSON avgF avoverNamesPerStrat
    pure avgF

averageOverNamesPerStrategyForExampleFile ::
       MonadIO m => Path Rel File -> m (Path Abs File)
averageOverNamesPerStrategyForExampleFile sourceF =
    jsonAverageFileWithComponents sourceF ["average-per-strategy"]

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

data AverageEvaluatorPerStrategyOutput = AverageEvaluatorPerStrategyOutput
    { averageEvaluatorPerStrategyEvaluatorName :: String
    , averageEvaluatorPerStrategyStrategy :: String
    , averageEvaluatorPerStrategyAverage :: AverageOutput
    } deriving (Show, Eq, Generic)

instance ToJSON AverageEvaluatorPerStrategyOutput where
    toJSON AverageEvaluatorPerStrategyOutput {..} =
        object
            [ "name" .= averageEvaluatorPerStrategyEvaluatorName
            , "strategy" .= averageEvaluatorPerStrategyStrategy
            , "average" .= averageEvaluatorPerStrategyAverage
            ]

averagesDir :: MonadIO m => m (Path Abs Dir)
averagesDir = (</> $(mkRelDir "averages")) <$> tmpDir

jsonAverageFileWithComponents ::
       MonadIO m => Path Rel File -> [String] -> m (Path Abs File)
jsonAverageFileWithComponents =
    fileInDirWithExtensionAndComponents averagesDir "json"

averageEvaluatorCsvLines :: [EvaluatorCsvLine] -> AverageOutput
averageEvaluatorCsvLines ecsvls =
    AverageOutput
    {averageOutputAverage = averageOrZero $ map eclEvaluatorOutput ecsvls}

newtype AverageOutput = AverageOutput
    { averageOutputAverage :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON AverageOutput where
    toJSON AverageOutput {..} = object ["average" .= averageOutputAverage]

averageOrZero :: [Double] -> Double
averageOrZero [] = 0
averageOrZero ls = sum ls / genericLength ls
