{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionOccurrencesInSameEquation
    ( dfrgOccurrencesInSameEquation
    ) where

import Import

import Data.Csv

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Discover.Utils as ES

import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Types

dfrgOccurrencesInSameEquation :: DistributionFromRawGatherer Occurrences
dfrgOccurrencesInSameEquation =
    DistributionFromRawGatherer
    { dfrgName = "occurrences-in-same-equation"
    , dfrgGatherFromPoints = occurrencesFromData
    , dfrgScript = scriptFile "occurrences-in-same-equation.r"
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
