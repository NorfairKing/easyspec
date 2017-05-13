{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Plots.SingleEvaluatorBar
    ( perExampleNameAndEvaluatorBarPlotFor
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover.Types as ES

import Development.Shake
import Development.Shake.Path

import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types

import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R

perExampleNameAndEvaluatorBarPlotFor ::
       ES.InputSpec -> ES.EasyName -> Evaluator -> Rules (Path Abs File)
perExampleNameAndEvaluatorBarPlotFor is name evaluator = do
    plotFile <- singleEvaluatorBarPlotFileForExampleAndName is name evaluator
    plotFile $%> do
        dependOnEvaluator evaluator
        dataFile <- dataFileForExampleAndName is name
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
    pure plotFile
