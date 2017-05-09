module EasySpec.Evaluate.Analyse where

import Import

import EasySpec.Evaluate.Analyse.Archive
import EasySpec.Evaluate.Analyse.Data
import EasySpec.Evaluate.Analyse.Plots
import EasySpec.Evaluate.Analyse.R

import Development.Shake

analyseRule :: String
analyseRule = "analyse"

analyseRules :: Rules ()
analyseRules = do
    rRules
    dataRules
    plotsRules
    archiveRules
    analyseRule ~> do
        need [dataRule, plotsRule]
        need [archiveRule] -- needs to be seperate
