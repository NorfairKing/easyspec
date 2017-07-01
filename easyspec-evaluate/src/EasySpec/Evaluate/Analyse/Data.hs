module EasySpec.Evaluate.Analyse.Data
    ( dataRule
    , dataRules
    ) where

import Import

import Development.Shake

import EasySpec.Evaluate.Analyse.Data.Averages
import EasySpec.Evaluate.Analyse.Data.Evaluated
import EasySpec.Evaluate.Analyse.Data.Raw

dataRule :: String
dataRule = "data"

dataRules :: Resource -> Rules ()
dataRules ghciResource = do
    rawDataRules ghciResource
    evaluatedDataRules
    averageDataRules
    dataRule ~> need [rawDataRule, averageDataRule]
