{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
    ( barPlotter
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.Plotter
import EasySpec.Evaluate.Analyse.R

barPlotter :: Plotter
barPlotter =
    "evaluator-bar"
    { plotterRulesGroupExampleNameEvaluator =
          Just perExampleNameAndEvaluatorBarPlotFor
    }

perExampleNameAndEvaluatorBarPlotFor ::
       Path Abs File
    -> Path Abs File
    -> GroupName
    -> Example
    -> ExampleFunction
    -> Evaluator
    -> Rules ()
perExampleNameAndEvaluatorBarPlotFor plotFile dataFile groupName is name evaluator =
    plotFile $%> do
        dependOnEvaluator evaluator
        dataFile <- dataFileForExampleAndName groupName is name
        singleEvaluatorBarScript <- singleEvaluatorBarAnalysisScript
        needP [dataFile]
        rscript
            singleEvaluatorBarScript
            [ toFilePath dataFile
            , toFilePath plotFile
            , toFilePath $ ES.inputSpecBaseDir is
            , toFilePath $ ES.inputSpecFile is
            , prettyPrint name
            , evaluatorName evaluator
            , prettyIndication $ evaluatorIndication evaluator
            ]

singleEvaluatorBarAnalysisScript :: MonadIO m => m (Path Abs File)
singleEvaluatorBarAnalysisScript = scriptFile "single_evaluator_bar.r"
