module EasySpec.Evaluate.Analyse where

import Import

import EasySpec.Evaluate.Analyse.Archive
import EasySpec.Evaluate.Analyse.Data
import EasySpec.Evaluate.Analyse.Plots

import Development.Shake
import Development.Shake.Path

analyseRule :: String
analyseRule = "analyse"

analyseRules :: Rules ()
analyseRules = do
    dataF <- dataRules
    plotsFs <- plotsRules
    zf <- analysisZipFileRules
    analyseRule ~> do
        needP (dataF : plotsFs)
        needP [zf]
