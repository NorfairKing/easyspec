{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionNrDifferentFunctions
    ( dfrgNrDifferentFunctions
    ) where

import Import

import Data.Csv

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Discover.Utils as ES

import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Types

dfrgNrDifferentFunctions :: DistributionFromRawGatherer NrDifferentFunctions
dfrgNrDifferentFunctions =
    DistributionFromRawGatherer
    { dfrgName = "nr-different-functions-per-equation"
    , dfrgGatherFromPoints = nrsDifferentFunctionsFromData
    , dfrgScript =
          scriptFile "distribution-nr-different-functions-per-equation.r"
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
