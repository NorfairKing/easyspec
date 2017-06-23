{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.ResultsPlots where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Csv

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Data.Content
import EasySpec.Evaluate.Analyse.Data.Files
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.R
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types

resultsPlotsFor ::
       (ToNamedRecord r, DefaultOrdered r, FromNamedRecord r)
    => EvaluationFunc r
    -> Rules [Path Abs File]
resultsPlotsFor ef = do
    enss <- examplesNamesAndStrategies
    mapM_ (uncurry3 $ individualCsvFilesFor ef) enss
    individualPlotFs <- mapM (uncurry3 $ individualPlotFor ef) enss
    mapM_ (perStrategyCsvFilesFor ef) signatureInferenceStrategies
    perStrategyPlotFs <-
        mapM (perStrategyPlotFor ef) signatureInferenceStrategies
    pure $ individualPlotFs ++ perStrategyPlotFs

data EvaluationFunc r = EvaluationFunc
    { evaluationFuncDir :: Path Rel Dir
    , evaluationFuncEval :: [EvaluationInputPoint] -> [r]
    , evaluationFuncIndividualMessage :: ES.InputSpec -> ES.EasyQName -> ES.SignatureInferenceStrategy -> Path Abs File -> String
    , evaluationFuncPerStrategyMessage :: ES.SignatureInferenceStrategy -> Path Abs File -> String
    , evaluationFuncPlotScript :: Path Abs File
    } deriving (Generic)

evaluationFuncIndividualCsvDataFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncIndividualCsvDataFileFor EvaluationFunc {..} e n s =
    csvDataFileWithComponents
        (evaluationFuncDir </> ES.inputSpecFile e)
        [prettyPrint n, ES.sigInfStratName s]

individualCsvFilesFor ::
       (ToNamedRecord r, DefaultOrdered r)
    => EvaluationFunc r
    -> ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
individualCsvFilesFor ef e n s = do
    csvF <- evaluationFuncIndividualCsvDataFileFor ef e n s
    csvF $%> do
        dat <- rawDataFrom e n s
        putLoud $ evaluationFuncIndividualMessage ef e n s csvF
        writeCSV csvF $ evaluationFuncEval ef [dat]
    pure csvF

evaluationFuncIndividualPngPlotFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncIndividualPngPlotFileFor EvaluationFunc {..} e n s =
    pngPlotFileWithComponents
        (evaluationFuncDir </> ES.inputSpecFile e)
        [prettyPrint n, ES.sigInfStratName s]

individualPlotFor ::
       EvaluationFunc r
    -> ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
individualPlotFor ef e n s = do
    plotF <- evaluationFuncIndividualPngPlotFileFor ef e n s
    plotF $%> do
        csvF <- evaluationFuncIndividualCsvDataFileFor ef e n s
        let scriptF = evaluationFuncPlotScript ef
        needP [csvF, scriptF]
        rscript
            scriptF
            [ toFilePath csvF
            , toFilePath plotF
            , "individual"
            , toFilePath $ ES.inputSpecFile e
            , prettyPrint n
            , ES.sigInfStratName s
            ]
    pure plotF

evaluationFuncPerStrategyCsvDataFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncPerStrategyCsvDataFileFor EvaluationFunc {..} s =
    csvDataFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "strategy"))
        [ES.sigInfStratName s]

perStrategyCsvFilesFor ::
       forall r. (ToNamedRecord r, DefaultOrdered r, FromNamedRecord r)
    => EvaluationFunc r
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
perStrategyCsvFilesFor ef s = do
    exns <- examplesAndNames
    csvFs <-
        mapM (\(e, n) -> evaluationFuncIndividualCsvDataFileFor ef e n s) exns
    resF <- evaluationFuncPerStrategyCsvDataFileFor ef s
    combineCSVFiles @r resF csvFs
    pure resF

evaluationFuncPerStrategyPngPlotFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncPerStrategyPngPlotFileFor EvaluationFunc {..} s =
    pngPlotFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "strategy"))
        [ES.sigInfStratName s]

perStrategyPlotFor ::
       EvaluationFunc r
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
perStrategyPlotFor ef s = do
    plotF <- evaluationFuncPerStrategyPngPlotFileFor ef s
    plotF $%> do
        csvF <- evaluationFuncPerStrategyCsvDataFileFor ef s
        let scriptF = evaluationFuncPlotScript ef
        needP [csvF, scriptF]
        rscript
            scriptF
            [ toFilePath csvF
            , toFilePath plotF
            , "per-strategy"
            , ES.sigInfStratName s
            ]
    pure plotF
