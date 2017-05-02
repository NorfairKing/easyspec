module EasySpec.Evaluate.Analyse where

import Import

import EasySpec.Evaluate.Analyse.Archive
import EasySpec.Evaluate.Analyse.Data
import EasySpec.Evaluate.Analyse.Plots

import Development.Shake

analyseRule :: String
analyseRule = "analyse"

analyseRules :: Rules ()
analyseRules = do
    dataRules
    plotsRules
    archiveRules
    analyseRule ~> need [dataRule, plotsRule, archiveRule]
