{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
    ( DistributionFromRawGatherer(..)
    , dfrgPlotter
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Csv

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.R
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types

data DistributionFromRawGatherer r = DistributionFromRawGatherer
    { dfrgName :: String
    , dfrgGatherFromPoints :: [EvaluationInputPoint] -> [r]
    , dfrgScript :: IO (Path Abs File)
    }

dfrgPlotter ::
       (ToNamedRecord r, DefaultOrdered r)
    => DistributionFromRawGatherer r
    -> Plotter
dfrgPlotter dfrg@DistributionFromRawGatherer {..} =
    (plotter dfrgName)
    { plotterRulesRawGroupStrategy = Just $ perGroupStrategy dfrg
    , plotterRulesRawGroupExampleNameStrategy =
          Just $ perGroupExampleNameStrategy dfrg
    }

perGroupStrategy ::
       (ToNamedRecord r, DefaultOrdered r)
    => DistributionFromRawGatherer r
    -> Path Abs File
    -> Action [EvaluationInputPoint]
    -> GroupName
    -> SignatureInferenceStrategy
    -> Rules ()
perGroupStrategy dfrg@DistributionFromRawGatherer {..} plotF getData g s = do
    csvF <- intermediateCSVFilePerGroupStrategy dfrg g s
    csvF $%> do
        dat <- getData
        putLoud $
            unwords
                [ "Running intermediate data gatherer"
                , dfrgName
                , "on raw data from"
                , "group"
                , g
                , "strategy"
                , strategyName s
                , "to build intermediate data file"
                , toFilePath csvF
                ]
        writeCSV csvF $ dfrgGatherFromPoints dat
    plotF $%> do
        needP [csvF]
        scriptF <- liftIO dfrgScript
        rscript
            scriptF
            [toFilePath csvF, toFilePath plotF, "per-strategy", strategyName s]

intermediateCSVFilePerGroupStrategy ::
       MonadIO m
    => DistributionFromRawGatherer r
    -> GroupName
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
intermediateCSVFilePerGroupStrategy dfrg g s =
    intermediateCSVFile dfrg [g] [strategyName s]

perGroupExampleNameStrategy ::
       (ToNamedRecord r, DefaultOrdered r)
    => DistributionFromRawGatherer r
    -> Path Abs File
    -> Action EvaluationInputPoint
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules ()
perGroupExampleNameStrategy dfrg@DistributionFromRawGatherer {..} plotF getData g e n s = do
    csvF <- intermediateCSVFilePerGroupExampleNameStrategy dfrg g e n s
    csvF $%> do
        dat <- getData
        putLoud $
            unwords
                [ "Running intermediate data gatherer"
                , dfrgName
                , "on raw data from"
                , "group"
                , g
                , "example"
                , exampleModule e
                , "focus"
                , prettyPrint n
                , "strategy"
                , strategyName s
                , "to build intermediate data file"
                , toFilePath csvF
                ]
        writeCSV csvF $ dfrgGatherFromPoints [dat]
    plotF $%> do
        needP [csvF]
        scriptF <- liftIO dfrgScript
        rscript
            scriptF
            [ toFilePath csvF
            , toFilePath plotF
            , "individual"
            , toFilePath $ ES.inputSpecFile e
            , prettyPrint n
            , strategyName s
            ]

intermediateCSVFilePerGroupExampleNameStrategy ::
       MonadIO m
    => DistributionFromRawGatherer r
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
intermediateCSVFilePerGroupExampleNameStrategy dfrg g e n s =
    intermediateCSVFile
        dfrg
        [g, exampleModule e, prettyPrint n]
        [strategyName s]

intermediateCSVFile ::
       MonadIO m
    => DistributionFromRawGatherer r
    -> [String]
    -> [String]
    -> m (Path Abs File)
intermediateCSVFile DistributionFromRawGatherer {..} dirComps fileComps =
    fileWithComponentsAndExtension
        ((</> $(mkRelDir "intermediate-from-raw")) <$> dataDir)
        (dfrgName : dirComps)
        fileComps
        "csv"
