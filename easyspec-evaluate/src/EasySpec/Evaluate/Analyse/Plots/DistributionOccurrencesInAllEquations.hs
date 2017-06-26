{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionOccurrencesInAllEquations where

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

plotsRulesDistributionDistributionOccurrencesInSameEquation ::
       Rules [Path Abs File]
plotsRulesDistributionDistributionOccurrencesInSameEquation = do
    plotF <- scriptFile "occurrences-in-same-equation.r"
    resultsPlotsFor
        EvaluationFunc
        { evaluationFuncDir = $(mkRelDir "occurrences-in-same-equation")
        , evaluationFuncEval = occurrencesFromData
        , evaluationFuncIndividualMessage =
              \e n s csvF ->
                  unwords
                      [ "Calculating the number of occurrences of a function in the same equation of the results of running easyspec on"
                      , toFilePath $ ES.inputSpecAbsFile e
                      , "with focus"
                      , show $ prettyPrint n
                      , "with signature inference strategy"
                      , show $ ES.sigInfStratName s
                      , "and writing the results to"
                      , toFilePath csvF
                      ]
        , evaluationFuncPerStrategyMessage =
              \s csvF ->
                  unwords
                      [ "Calculating the number of  occurrences of a function in the same equation of the results of running easyspec on all examples and names, but with signature inference strategy"
                      , show $ ES.sigInfStratName s
                      , "and writing the results to"
                      , toFilePath csvF
                      ]
        , evaluationFuncPlotScript = plotF
        }

occurrencesFromData :: [EvaluationInputPoint] -> [Occurrences]
occurrencesFromData dats = map Occurrences $ concatMap occurrencesFrom dats

occurrencesFrom :: EvaluationInputPoint -> [Int]
occurrencesFrom ei =
    filter (/= 0) $
    ES.occurrencesEq <$> ES.ordNub (eipFunc ei : map ES.idName (eipScope ei)) <*>
    eipDiscoveredEqs ei

newtype Occurrences =
    Occurrences Int
    deriving (Show, Eq, Generic)

instance ToNamedRecord Occurrences where
    toNamedRecord (Occurrences i) = namedRecord ["occurrences" .= i]

instance DefaultOrdered Occurrences where
    headerOrder _ = header ["occurrences"]

instance FromNamedRecord Occurrences where
    parseNamedRecord r = Occurrences <$> r .: "occurrences"
