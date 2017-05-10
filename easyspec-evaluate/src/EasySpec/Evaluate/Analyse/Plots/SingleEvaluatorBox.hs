{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBox
    ( perExampleAndEvaluatorAverageBoxPlotFor
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
            ]
    pure plotF
