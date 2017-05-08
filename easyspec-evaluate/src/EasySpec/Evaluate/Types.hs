{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Evaluate.Types where

import Import hiding (Alt)

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Csv hiding (Name)

import Language.Haskell.Exts.Syntax

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Evaluate.Evaluate.Evaluator.Types as ES

import qualified EasySpec.Evaluate.HaskellJSON ()

instance ToJSON ES.InputSpec where
    toJSON ES.InputSpec {..} =
        JSON.object
            ["base dir" JSON..= inputSpecBaseDir, "file" JSON..= inputSpecFile]

instance FromJSON ES.InputSpec where
    parseJSON =
        JSON.withObject "InputSpec" $ \o ->
            ES.InputSpec <$> o JSON..: "base dir" <*> o JSON..: "file"

instance ToJSON ES.EasyEq where
    toJSON (ES.EasyEq e1 e2) =
        JSON.object ["left-hand side" JSON..= e1, "right-hand side" JSON..= e2]

instance FromJSON ES.EasyEq where
    parseJSON =
        JSON.withObject "EasyEq" $ \o ->
            ES.EasyEq <$> o JSON..: "left-hand side" <*>
            o JSON..: "right-hand side"

instance ToJSON (ES.Impl ()) where
    toJSON i =
        case i of
            ES.Impl ms -> toJSON $ FunBind () ms

instance FromJSON (ES.Impl ()) where
    parseJSON v = do
        d <- parseJSON v
        case d of
            FunBind () ms -> pure $ ES.Impl ms
            _ -> mempty

instance ToJSON (ES.Id ()) where
    toJSON ES.Id {..} =
        JSON.object
            [ "name" JSON..= idName
            , "type" JSON..= idType
            , "implementation" JSON..= idImpl
            ]

instance FromJSON (ES.Id ()) where
    parseJSON =
        JSON.withObject "EasyId" $ \o ->
            ES.Id <$> o JSON..: "name" <*> o JSON..: "type" <*>
            o JSON..: "implementation"

data EvaluationInputPoint = EvaluationInputPoint
    { eipInputSpec :: ES.InputSpec
    , eipStrat :: String
    , eipFunc :: ES.EasyName
    , eipScope :: [ES.EasyId]
    , eipDiscoveredEqs :: [ES.EasyEq]
    , eipRuntime :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON EvaluationInputPoint where
    toJSON EvaluationInputPoint {..} =
        JSON.object
            [ "input spec" JSON..= eipInputSpec
            , "strategy" JSON..= eipStrat
            , "focus" JSON..= eipFunc
            , "scope" JSON..= eipScope
            , "discovered equations" JSON..= eipDiscoveredEqs
            , "runtime" JSON..= eipRuntime
            ]

instance FromJSON EvaluationInputPoint where
    parseJSON =
        JSON.withObject "EvaluationInputPoint" $ \o ->
            EvaluationInputPoint <$> o JSON..: "input spec" <*>
            (o JSON..: "strategy") <*>
            (o JSON..: "focus") <*>
            (o JSON..: "scope") <*>
            (o JSON..: "discovered equations") <*>
            (o JSON..: "runtime")

data EvaluatorCsvLine = EvaluatorCsvLine
    { eclBaseDir :: Path Abs Dir
    , eclFile :: Path Rel File
    , eclStratName :: String
    , eclFocusFuncName :: String
    , eclEvaluatorName :: ES.EvaluatorName
    , eclEvaluatorOutput :: Maybe Double
    , eclEvaluatorUnit :: String
    , eclEvaluatorQuantity :: String
    , eclEvaluatorPrettyOutput :: String
    } deriving (Show, Eq)

instance FromNamedRecord EvaluatorCsvLine where
    parseNamedRecord r =
        EvaluatorCsvLine <$> p parseAbsDir "base-dir" <*> p parseRelFile "file" <*>
        r .: "strategy" <*>
        r .: "focus" <*>
        r .: "evaluator" <*>
        r .: "output" <*>
        r .: "unit" <*>
        r .: "quantity" <*>
        r .: "pretty output"
      where
        p parser key = do
            v <- r .: key
            case parser v of
                Left err ->
                    fail $
                    unwords
                        [ "Parsing of field"
                        , show key
                        , "failed with error:"
                        , show err
                        ]
                Right res -> pure res

instance ToNamedRecord EvaluatorCsvLine where
    toNamedRecord EvaluatorCsvLine {..} =
        namedRecord
            [ "base-dir" .= toFilePath eclBaseDir
            , "file" .= toFilePath eclFile
            , "strategy" .= eclStratName
            , "focus" .= eclFocusFuncName
            , "evaluator" .= eclEvaluatorName
            , "output" .= eclEvaluatorOutput
            , "unit" .= eclEvaluatorUnit
            , "quantity" .= eclEvaluatorQuantity
            , "pretty output" .= eclEvaluatorPrettyOutput
            ]

instance DefaultOrdered EvaluatorCsvLine where
    headerOrder _ =
        header
            [ "base-dir"
            , "file"
            , "strategy"
            , "focus"
            , "evaluator"
            , "output"
            , "unit"
            , "quantity"
            , "pretty output"
            ]
