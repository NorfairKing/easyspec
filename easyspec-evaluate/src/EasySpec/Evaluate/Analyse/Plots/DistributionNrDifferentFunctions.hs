{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)


import Data.Csv

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Content
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types

plotsRulesDistributionNrDifferentFunctions :: Rules [Path Abs File]
plotsRulesDistributionNrDifferentFunctions = do
    plotFile <- nrDifferentFunctionsCsvFileRulesForAllRules
    enss <- examplesNamesAndStrategies
    csvFs <- mapM (uncurry3 nrDifferentFunctionsCsvFileRulesFor) enss
    csvFs' <-
        mapM
            nrDifferentFunctionsCsvFileRulesForStrategyRules
            signatureInferenceStrategies
    pure $ plotFile : csvFs ++ csvFs'

nrDifferentFunctionsCsvFileRulesForAllRules :: Rules (Path Abs File)
nrDifferentFunctionsCsvFileRulesForAllRules = do
    csvFs <-
        mapM
            csvFileForDistributionNrDifferentFunctionsForStrategy
            signatureInferenceStrategies
    resF <- csvFileForDistributionNrDifferentFunctions
    combineCSVFiles @NrDifferentFunctions resF csvFs
    plotFile <- plotFileForDistributionNrDifferentFunctions
    plotFile $%> do
        needP [resF]
        scriptF <- distributionNrDifferentFunctionsAnalysisScript
        rscript scriptF [toFilePath resF, toFilePath plotFile]
    pure plotFile

nrDifferentFunctionsCsvFileRulesForStrategyRules ::
       ES.SignatureInferenceStrategy -> Rules (Path Abs File)
nrDifferentFunctionsCsvFileRulesForStrategyRules s = do
    exns <- examplesAndNames
    fs <-
        forM exns $ \(example, name) ->
            csvFileForDistributionNrDifferentFunctionsFor example name s
    resF <- csvFileForDistributionNrDifferentFunctionsForStrategy s
    combineCSVFiles @NrDifferentFunctions resF fs
    pure resF

nrDifferentFunctionsCsvFileRulesFor ::
       ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
nrDifferentFunctionsCsvFileRulesFor e n s = do
    csvF <- csvFileForDistributionNrDifferentFunctionsFor e n s
    csvF $%> do
        dat <- rawDataFrom e n s
        putLoud $
            unwords
                [ "Calculating the number of different functions per equation in the results of running easyspec on"
                , toFilePath $ ES.inputSpecAbsFile e
                , "with focus"
                , show $ prettyPrint n
                , "with signature inference strategy"
                , show $ ES.sigInfStratName s
                , "and writing the results to"
                , toFilePath csvF
                ]
        writeCSV csvF $ nrsDifferentFunctionsFromData [dat]
    pure csvF

nrsDifferentFunctionsFromData ::
       [EvaluationInputPoint] -> [NrDifferentFunctions]
nrsDifferentFunctionsFromData dats =
    map NrDifferentFunctions $ concatMap nrsDifferentFunctionsFrom dats

nrsDifferentFunctionsFrom :: EvaluationInputPoint -> [Int]
nrsDifferentFunctionsFrom ei =
    map (nrDifferentFunctionsFrom (eipFunc ei : map ES.idName (eipScope ei)))
        (eipDiscoveredEqs ei)

nrDifferentFunctionsFrom :: [ES.EasyQName] -> ES.EasyEq -> Int
nrDifferentFunctionsFrom ns eq = length $ filter (`ES.mentionsEq` eq) ns

newtype NrDifferentFunctions =
    NrDifferentFunctions Int
    deriving (Show, Eq, Generic)

instance ToNamedRecord NrDifferentFunctions where
    toNamedRecord (NrDifferentFunctions i) =
        namedRecord ["nrDifferentFunctions" .= i]

instance DefaultOrdered NrDifferentFunctions where
    headerOrder _ = header ["nrDifferentFunctions"]

instance FromNamedRecord NrDifferentFunctions where
    parseNamedRecord r = NrDifferentFunctions <$> r .: "nrDifferentFunctions"
