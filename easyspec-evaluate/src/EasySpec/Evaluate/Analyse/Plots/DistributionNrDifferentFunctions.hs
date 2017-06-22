{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions where

import Import

import Data.Csv

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types

plotsRulesDistributionNrDifferentFunctions :: Rules (Path Abs File)
plotsRulesDistributionNrDifferentFunctions = do
    csvF <- csvFileForDistributionNrDifferentFunctions
    csvF $%> do
        tups <-
            fmap concat $
            forM examples $ \example -> do
                names <- liftIO $ namesInSource example
                pure $ map ((,) example) names
        let trips =
                [(e, n, s) | (e, n) <- tups, s <- signatureInferenceStrategies]
        datFs <- mapM (\(e, n, s) -> rawDataFileFor e n s) trips
        needP datFs
        dats <- mapM readJSON datFs
        writeCSV csvF $ nrsDifferentFunctionsFromData dats
    plotFile <- plotFileForDistributionNrDifferentFunctions
    plotFile $%> do
        needP [csvF]
        scriptF <- distributionNrDifferentFunctionsAnalysisScript
        rscript scriptF [toFilePath csvF, toFilePath plotFile]
    pure plotFile

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
