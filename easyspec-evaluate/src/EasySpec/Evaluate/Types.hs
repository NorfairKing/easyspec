{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Evaluate.Types where

import Import hiding (Alt)

import Language.Haskell.Exts.Syntax

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.Csv hiding (Name)

import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Evaluate.Evaluate.Evaluator.Types as ES

-- JSON instances for all of the syntax
$(fmap concat $
  mapM
      (deriveJSON defaultOptions)
      [ ''Activation
      , ''Alt
      , ''Annotation
      , ''Assoc
      , ''Asst
      , ''BangType
      , ''Binds
      , ''BooleanFormula
      , ''Boxed
      , ''Bracket
      , ''CName
      , ''CallConv
      , ''ClassDecl
      , ''ConDecl
      , ''Context
      , ''DataOrNew
      , ''Decl
      , ''DeclHead
      , ''Deriving
      , ''EWildcard
      , ''Exp
      , ''ExportSpec
      , ''ExportSpecList
      , ''FieldDecl
      , ''FieldUpdate
      , ''FunDep
      , ''GadtDecl
      , ''GuardedRhs
      , ''IPBind
      , ''IPName
      , ''ImportDecl
      , ''ImportSpec
      , ''ImportSpecList
      , ''InjectivityInfo
      , ''InstDecl
      , ''InstHead
      , ''InstRule
      , ''Kind
      , ''Literal
      , ''Match
      , ''Module
      , ''ModuleHead
      , ''ModuleName
      , ''ModulePragma
      , ''Name
      , ''Namespace
      , ''Op
      , ''Overlap
      , ''PXAttr
      , ''Pat
      , ''PatField
      , ''PatternSynDirection
      , ''Promoted
      , ''QName
      , ''QOp
      , ''QualConDecl
      , ''QualStmt
      , ''RPat
      , ''RPatOp
      , ''ResultSig
      , ''Rhs
      , ''Role
      , ''Rule
      , ''RuleVar
      , ''Safety
      , ''Sign
      , ''SpecialCon
      , ''Splice
      , ''Stmt
      , ''Tool
      , ''TyVarBind
      , ''Type
      , ''TypeEqn
      , ''Unpackedness
      , ''WarningText
      , ''XAttr
      , ''XName
      ])

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
        JSON.object
            [ "left-hand side" JSON..= JSON.genericToJSON JSON.defaultOptions e1
            , "right-hand side" JSON..=
              JSON.genericToJSON JSON.defaultOptions e2
            ]

instance FromJSON ES.EasyEq where
    parseJSON =
        JSON.withObject "EasyEq" $ \o ->
            ES.EasyEq <$> (o JSON..: "left-hand side") <*>
            (o JSON..: "right-hand side")

data EvaluationInputPoint = EvaluationInputPoint
    { eipInputSpec :: ES.InputSpec
    , eipFunc :: ES.EasyName
    , eipStrat :: String
    , eipDiscoveredEqs :: [ES.EasyEq]
    , eipRuntime :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON EvaluationInputPoint where
    toJSON EvaluationInputPoint {..} =
        JSON.object
            [ "input spec" JSON..= eipInputSpec
            , "focus" JSON..= eipFunc
            , "strategy" JSON..= eipStrat
            , "discovered equations" JSON..= eipDiscoveredEqs
            , "runtime" JSON..= eipRuntime
            ]

instance FromJSON EvaluationInputPoint where
    parseJSON =
        JSON.withObject "EvaluationInputPoint" $ \o ->
            EvaluationInputPoint <$> o JSON..: "input spec" <*>
            (o JSON..: "focus") <*>
            (o JSON..: "strategy") <*>
            (o JSON..: "discovered equations") <*>
            (o JSON..: "runtime")

data EvaluatorCsvLine = EvaluatorCsvLine
    { eclBaseDir :: Path Abs Dir
    , eclFile :: Path Rel File
    , eclStratName :: String
    , eclFocusFuncName :: String
    , eclEvaluatorName :: ES.EvaluatorName
    , eclEvaluatorOutput :: Double
    } deriving (Show, Eq)

instance FromNamedRecord EvaluatorCsvLine where
    parseNamedRecord r =
        EvaluatorCsvLine <$> p parseAbsDir "base-dir" <*> p parseRelFile "file" <*>
        r .: "strategy" <*>
        r .: "focus" <*>
        r .: "evaluator" <*>
        r .: "output"
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
            ]

instance DefaultOrdered EvaluatorCsvLine where
    headerOrder _ =
        header ["base-dir", "file", "strategy", "focus", "evaluator", "output"]
