{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.Exts.Gen where

import TestImport hiding (Alt)

import Language.Haskell.Exts.Syntax

instance GenUnchecked l => GenUnchecked (Activation l)

instance GenUnchecked l => GenUnchecked (Alt l)

instance GenUnchecked l => GenUnchecked (Annotation l)

instance GenUnchecked l => GenUnchecked (Assoc l)

instance GenUnchecked l => GenUnchecked (Asst l)

instance GenUnchecked l => GenUnchecked (BangType l)

instance GenUnchecked l => GenUnchecked (Binds l)

instance GenUnchecked l => GenUnchecked (BooleanFormula l)

instance GenUnchecked Boxed

instance GenUnchecked l => GenUnchecked (Bracket l)

instance GenUnchecked l => GenUnchecked (CName l)

instance GenUnchecked l => GenUnchecked (CallConv l)

instance GenUnchecked l => GenUnchecked (ClassDecl l)

instance GenUnchecked l => GenUnchecked (ConDecl l)

instance GenUnchecked l => GenUnchecked (Context l)

instance GenUnchecked l => GenUnchecked (DataOrNew l)

instance GenUnchecked l => GenUnchecked (Decl l)

instance GenUnchecked l => GenUnchecked (DeclHead l)

instance GenUnchecked l => GenUnchecked (Deriving l)

instance GenUnchecked l => GenUnchecked (EWildcard l)

instance GenUnchecked l => GenUnchecked (Exp l)

instance GenUnchecked l => GenUnchecked (ExportSpec l)

instance GenUnchecked l => GenUnchecked (ExportSpecList l)

instance GenUnchecked l => GenUnchecked (FieldDecl l)

instance GenUnchecked l => GenUnchecked (FieldUpdate l)

instance GenUnchecked l => GenUnchecked (FunDep l)

instance GenUnchecked l => GenUnchecked (GadtDecl l)

instance GenUnchecked l => GenUnchecked (GuardedRhs l)

instance GenUnchecked l => GenUnchecked (IPBind l)

instance GenUnchecked l => GenUnchecked (IPName l)

instance GenUnchecked l => GenUnchecked (ImportDecl l)

instance GenUnchecked l => GenUnchecked (ImportSpec l)

instance GenUnchecked l => GenUnchecked (ImportSpecList l)

instance GenUnchecked l => GenUnchecked (InjectivityInfo l)

instance GenUnchecked l => GenUnchecked (InstDecl l)

instance GenUnchecked l => GenUnchecked (InstHead l)

instance GenUnchecked l => GenUnchecked (InstRule l)

instance GenUnchecked l => GenUnchecked (Kind l)

instance GenUnchecked l => GenUnchecked (Literal l)

instance GenUnchecked l => GenUnchecked (Match l)

instance GenUnchecked l => GenUnchecked (Module l)

instance GenUnchecked l => GenUnchecked (ModuleHead l)

instance GenUnchecked l => GenUnchecked (ModuleName l)

instance GenUnchecked l => GenUnchecked (ModulePragma l)

instance GenUnchecked l => GenUnchecked (Name l)

instance GenUnchecked l => GenUnchecked (Namespace l)

instance GenUnchecked l => GenUnchecked (Op l)

instance GenUnchecked l => GenUnchecked (Overlap l)

instance GenUnchecked l => GenUnchecked (PXAttr l)

instance GenUnchecked l => GenUnchecked (Pat l)

instance GenUnchecked l => GenUnchecked (PatField l)

instance GenUnchecked l => GenUnchecked (PatternSynDirection l)

instance GenUnchecked l => GenUnchecked (Promoted l)

instance GenUnchecked l => GenUnchecked (QName l)

instance GenUnchecked l => GenUnchecked (QOp l)

instance GenUnchecked l => GenUnchecked (QualConDecl l)

instance GenUnchecked l => GenUnchecked (QualStmt l)

instance GenUnchecked l => GenUnchecked (RPat l)

instance GenUnchecked l => GenUnchecked (RPatOp l)

instance GenUnchecked l => GenUnchecked (ResultSig l)

instance GenUnchecked l => GenUnchecked (Rhs l)

instance GenUnchecked l => GenUnchecked (Role l)

instance GenUnchecked l => GenUnchecked (Rule l)

instance GenUnchecked l => GenUnchecked (RuleVar l)

instance GenUnchecked l => GenUnchecked (Safety l)

instance GenUnchecked l => GenUnchecked (Sign l)

instance GenUnchecked l => GenUnchecked (SpecialCon l)

instance GenUnchecked l => GenUnchecked (Splice l)

instance GenUnchecked l => GenUnchecked (Stmt l)

instance GenUnchecked Tool

instance GenUnchecked l => GenUnchecked (TyVarBind l)

instance GenUnchecked l => GenUnchecked (Type l)

instance GenUnchecked l => GenUnchecked (TypeEqn l)

instance GenUnchecked l => GenUnchecked (Unpackedness l)

instance GenUnchecked l => GenUnchecked (WarningText l)

instance GenUnchecked l => GenUnchecked (XAttr l)

instance GenUnchecked l => GenUnchecked (XName l)
