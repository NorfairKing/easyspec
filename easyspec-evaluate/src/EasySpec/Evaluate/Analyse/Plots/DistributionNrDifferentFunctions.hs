{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Csv

import Development.Shake

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Discover.Utils as ES

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.ResultsPlots
import EasySpec.Evaluate.Types

plotsRulesDistributionNrDifferentFunctions :: Rules [Path Abs File]
plotsRulesDistributionNrDifferentFunctions = do
    plotF <- scriptFile "distribution-nr-different-functions-per-equation.r"
    resultsPlotsFor
        EvaluationFunc
        { evaluationFuncDir =
              $(mkRelDir "distribution-nr-different-functions-per-equation")
        , evaluationFuncEval = nrsDifferentFunctionsFromData
        , evaluationFuncIndividualMessage =
              \_ e n s csvF ->
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
        , evaluationFuncPerStrategyMessage =
              \_ s csvF ->
                  unwords
                      [ "Calculating the number of different functions per equation in the results of running easyspec on all examples and names, but with signature inference strategy"
                      , show $ ES.sigInfStratName s
                      , "and writing the results to"
                      , toFilePath csvF
                      ]
        , evaluationFuncPlotScript = plotF
        }

nrsDifferentFunctionsFromData ::
       [EvaluationInputPoint] -> [NrDifferentFunctions]
nrsDifferentFunctionsFromData dats =
    map NrDifferentFunctions $ concatMap nrsDifferentFunctionsFrom dats

nrsDifferentFunctionsFrom :: EvaluationInputPoint -> [Int]
nrsDifferentFunctionsFrom ei =
    map (nrDifferentFunctionsFrom
             (ES.ordNub $ eipFunc ei : map ES.idName (eipScope ei)))
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
