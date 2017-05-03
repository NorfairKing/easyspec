module EasySpec.Evaluate.Analyse.Data
    ( dataRule
    , dataRules
    ) where

import Import

import Development.Shake

import EasySpec.Evaluate.Analyse.Data.Averages
import EasySpec.Evaluate.Analyse.Data.Raw

dataRule :: String
dataRule = "data"

dataRules :: Rules ()
dataRules = do
    rawDataRules
    averageDataRules
    dataRule ~> need [rawDataRule, averageDataRule]
