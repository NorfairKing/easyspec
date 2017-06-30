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
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Analyse.Utils

import EasySpec.Evaluate.Analyse.Data.Files

rawDataRule :: String
rawDataRule = "raw-data"

rawDataRules :: Resource -> Rules ()
rawDataRules ghciResource = do
    datFs <- groupsExamplesNamesAndStrategies >>=
        mapM (uncurry4 $ rawDataRulesForGroupFileNameAndStrat ghciResource)
    rawDataRule ~> needP datFs

rawDataRulesForGroupFileNameAndStrat ::
       Resource
    -> GroupName
    -> Example
    -> ExampleFunction
    -> SignatureInferenceStrategy
    -> Rules (Path Abs File)
rawDataRulesForGroupFileNameAndStrat ghciResource groupName is name infStrat = do
    jsonF <- rawDataFileFor groupName is name infStrat
    jsonF $%> do
        sourceDir <- getEasyspecSourceDir
        let relevantFiles =
                map (sourceDir </>) $ ES.sigInfRelevantSources infStrat
        let absFile = ES.inputSpecAbsFile is
        needP $ absFile : relevantFiles
        ip <-
            withResource ghciResource 1 $ do
                putLoud $
                    unwords
                        [ "Building raw data file"
                        , toFilePath jsonF
                        , "by running 'easyspec-evaluate' on"
                        , toFilePath absFile
                        , "with focus:"
                        , show $ prettyPrint name
                        , "and signature inference strategy:"
                        , show $ ES.sigInfStratName infStrat
                        ]
                liftIO $ getEvaluationInputPoint is name infStrat
        writeJSON jsonF ip
    pure jsonF
