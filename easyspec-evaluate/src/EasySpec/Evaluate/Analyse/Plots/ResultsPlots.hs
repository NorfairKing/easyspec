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
resultsPlotsFor ef = pure []
    -- enss <- groupsExamplesNamesAndStrategies
    -- individualPlotFs <-
    --     forM enss $ \quad -> do
    --         void $ uncurry4 (individualCsvFilesFor ef) quad
    --         uncurry4 (individualPlotFor ef) quad
    -- perStrategyPlotFs <-
    --     forM ((,) <$> groups <*> signatureInferenceStrategies) $ \(g, strat) -> do
    --         void $ perStrategyCsvFilesFor ef g strat
    --         perStrategyPlotFor ef g strat
    -- pure $ individualPlotFs ++ perStrategyPlotFs

data EvaluationFunc r = EvaluationFunc
    { evaluationFuncDir :: Path Rel Dir
    , evaluationFuncEval :: [EvaluationInputPoint] -> [r]
    , evaluationFuncIndividualMessage :: GroupName -> ES.InputSpec -> ES.EasyQName -> ES.SignatureInferenceStrategy -> Path Abs File -> String
    , evaluationFuncPerStrategyMessage :: GroupName -> ES.SignatureInferenceStrategy -> Path Abs File -> String
    , evaluationFuncPlotScript :: Path Abs File
    } deriving (Generic)

evaluationFuncIndividualCsvDataFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncIndividualCsvDataFileFor EvaluationFunc {..} g e n s =
    csvDataFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "data"))
        [g, exampleModule e, prettyPrint n, ES.sigInfStratName s]

individualCsvFilesFor ::
       (ToNamedRecord r, DefaultOrdered r)
    => EvaluationFunc r
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules (Path Abs File)
individualCsvFilesFor ef g e n s = do
    csvF <- evaluationFuncIndividualCsvDataFileFor ef g e n s
    csvF $%> do
        dat <- rawDataFrom g e n s
        putLoud $ evaluationFuncIndividualMessage ef g e n s csvF
        writeCSV csvF $ evaluationFuncEval ef [dat]
    pure csvF

evaluationFuncIndividualPngPlotFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncIndividualPngPlotFileFor EvaluationFunc {..} g e n s =
    pngPlotFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "plot"))
        [g, exampleModule e, prettyPrint n, ES.sigInfStratName s]

individualPlotFor ::
       EvaluationFunc r
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules (Path Abs File)
individualPlotFor ef g e n s = do
    plotF <- evaluationFuncIndividualPngPlotFileFor ef g e n s
    plotF $%> do
        csvF <- evaluationFuncIndividualCsvDataFileFor ef g e n s
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
    -> GroupName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncPerStrategyCsvDataFileFor EvaluationFunc {..} g s =
    csvDataFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "strategy"))
        [g, ES.sigInfStratName s]

perStrategyCsvFilesFor ::
       forall r. (ToNamedRecord r, DefaultOrdered r, FromNamedRecord r)
    => EvaluationFunc r
    -> GroupName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
perStrategyCsvFilesFor ef g s = do
    exns <- examplesAndNames
    csvFs <-
        mapM (\(e, n) -> evaluationFuncIndividualCsvDataFileFor ef g e n s) exns
    resF <- evaluationFuncPerStrategyCsvDataFileFor ef g s
    combineCSVFiles @r resF csvFs
    pure resF

evaluationFuncPerStrategyPngPlotFileFor ::
       MonadIO m
    => EvaluationFunc r
    -> GroupName
    -> ES.SignatureInferenceStrategy
    -> m (Path Abs File)
evaluationFuncPerStrategyPngPlotFileFor EvaluationFunc {..} g s =
    pngPlotFileWithComponents
        (evaluationFuncDir </> $(mkRelFile "strategy"))
        [g, ES.sigInfStratName s]

perStrategyPlotFor ::
       EvaluationFunc r
    -> GroupName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
perStrategyPlotFor ef g s = do
    plotF <- evaluationFuncPerStrategyPngPlotFileFor ef g s
    plotF $%> do
        csvF <- evaluationFuncPerStrategyCsvDataFileFor ef g s
        let scriptF = evaluationFuncPlotScript ef
        needP [csvF]
        rscript
            scriptF
            [ toFilePath csvF
            , toFilePath plotF
            , "per-strategy"
            , ES.sigInfStratName s
            ]
    pure plotF
