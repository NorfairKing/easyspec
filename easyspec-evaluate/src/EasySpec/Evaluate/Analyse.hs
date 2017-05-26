module EasySpec.Evaluate.Analyse where

import Import

import EasySpec.Evaluate.Analyse.Archive
import EasySpec.Evaluate.Analyse.Data
import EasySpec.Evaluate.Analyse.Hackage
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
    hackageRules
    analyseRule ~> do
        need [dataRule, plotsRule, hackageRule]
        need [archiveRule] -- needs to be seperate
