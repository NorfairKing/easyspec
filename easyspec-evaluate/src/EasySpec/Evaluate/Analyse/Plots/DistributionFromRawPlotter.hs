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

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Content
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
    { plotterRulesRawGroupExampleNameStrategy =
          Just $ perGroupExampleNameStrategy dfrg
    }

perGroupExampleNameStrategy ::
       (ToNamedRecord r, DefaultOrdered r)
    => DistributionFromRawGatherer r
    -> Path Abs File
    -> Path Abs File
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules ()
perGroupExampleNameStrategy dfrg@DistributionFromRawGatherer {..} plotF dataF g e n s = do
    csvF <- intermediateCSVFilePerGroupExampleNameStrategy dfrg g e n s
    csvF $%> do
        needP [dataF]
        dat <- readJSON dataF
        putLoud $
            unwords
                [ "Running intermediate data gatherer"
                , dfrgName
                , "on raw data file"
                , toFilePath dataF
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
