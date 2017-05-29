{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( perExampleAndEvaluatorAverageBoxPlotFor
    , perEvaluatorGlobalAverageBoxPlotFor
    ) where

import Import

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

perExampleAndEvaluatorAverageBoxPlotFor ::
       ES.InputSpec -> Evaluator -> Rules (Path Abs File)
perExampleAndEvaluatorAverageBoxPlotFor is evaluator = do
    plotF <- singleEvaluatorAverageBoxPlotFileForExample is evaluator
    plotF $%> do
        dependOnEvaluator evaluator
        dataF <- dataFileForExample is
        needP [dataF]
        scriptF <- singleEvaluatorAverageBoxAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , toFilePath $ ES.inputSpecBaseDir is
            , toFilePath $ ES.inputSpecFile is
            , evaluatorName evaluator
            , prettyIndication $ evaluatorIndication evaluator
            ]
    pure plotF

perEvaluatorGlobalAverageBoxPlotFor :: Evaluator -> Rules (Path Abs File)
perEvaluatorGlobalAverageBoxPlotFor evaluator = do
    plotF <- singleEvaluatorAverageGlobalBoxPlotFileForExample evaluator
    plotF $%> do
        dependOnEvaluator evaluator
        dataF <- allDataFile
        needP [dataF]
        scriptF <- singleEvaluatorAverageBoxGlobalAnalysisScript
        rscript
            scriptF
            [ toFilePath dataF
            , toFilePath plotF
            , evaluatorName evaluator
            , prettyIndication $ evaluatorIndication evaluator
            ]
    pure plotF
