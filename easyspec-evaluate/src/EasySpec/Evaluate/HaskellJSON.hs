{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Evaluate.HaskellJSON where

import Import hiding (Alt)

import Language.Haskell.Exts.Syntax

import Data.Aeson.TH

-- JSON instances for all of the syntax
$(concat <$>
  mapM (deriveJSON defaultOptions)
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
