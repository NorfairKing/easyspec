module EasySpec.Evaluate.Analyse.Data
    ( dataRule
    , dataRules
    ) where

import Import

import Development.Shake

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Averages
import EasySpec.Evaluate.Analyse.Data.Raw
import EasySpec.Evaluate.Analyse.Data.Types

dataRule :: String
dataRule = "data"

dataRules :: Rules ()
dataRules = do
    addEquationsOracle
    rawDataRules
    averageDataRules
    dataRule ~> need [rawDataRule, averageDataRule]

addEquationsOracle :: Rules ()
addEquationsOracle =
    void $ addOracle $ \(Equations ()) -> pure $ map evaluatorName evaluators
