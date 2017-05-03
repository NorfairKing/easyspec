{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EasySpec.Evaluate.Analyse.Data.Raw
    ( rawDataRule
    , rawDataRules
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Development.Shake
import Development.Shake.Path

import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Files

rawDataRule :: String
rawDataRule = "raw-data"

rawDataRules :: Rules ()
rawDataRules = do
    ghciResource <- newResource "ghci" 1
    es <- examples
    csvFs <- mapM (dataRulesForExample ghciResource) es
    combF <- allDataFile
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    rawDataRule ~> needP [combF]

dataRulesForExample :: Resource -> Path Rel File -> Rules (Path Abs File)
dataRulesForExample ghciResource sourceF = do
    absSourceF <- absExampleFile sourceF
    names <- namesInSource absSourceF
    csvFs <- forM names $ rulesForFileAndName ghciResource sourceF
    combF <- dataFileForExample sourceF
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileAndName ::
       Resource -> Path Rel File -> ES.EasyName -> Rules (Path Abs File)
rulesForFileAndName ghciResource sourceF name = do
    csvFs <-
        forM signatureInferenceStrategies $
        rulesForFileNameAndStrat ghciResource sourceF name
    combF <- dataFileForExampleAndName sourceF name
    combineCSVFiles @EvaluatorCsvLine combF csvFs
    pure combF

rulesForFileNameAndStrat ::
       Resource
    -> Path Rel File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> Rules (Path Abs File)
rulesForFileNameAndStrat ghciResource sourceF name infStrat = do
    absSourceF <- absExampleFile sourceF
    csvF <- dataFileFor sourceF name infStrat
    csvF $%> do
        needP [absSourceF]
        ip <-
            withResource ghciResource 1 $ do
                putLoud $
                    unwords
                        [ "Building data file"
                        , toFilePath csvF
                        , "by running 'easyspec-evaluate' on"
                        , toFilePath absSourceF
                        , "with focus:"
                        , prettyPrint name
                        , "and signature inference strategy:"
                        , ES.sigInfStratName infStrat
                        ]
                liftIO $ getEvaluationInputPoint absSourceF name infStrat
        writeCSV csvF $ evaluationInputPointCsvLines ip
    pure csvF
