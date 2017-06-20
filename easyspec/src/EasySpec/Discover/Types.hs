{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Discover.Types where

import Import

import Path.Internal (Path(Path))

import Text.Show.Pretty (ppShow)

import qualified Language.Haskell.TH.Syntax as TH (Lift)

import Language.Haskell.Exts.Pretty as H
import Language.Haskell.Exts.Syntax as H

-- FIXME
-- It is really annoying that these aren't in 'path' yet.
-- Once they are, we can remove this, although the lift instances
-- will probably not make it in there.
deriving instance Data Abs

deriving instance Data Rel

deriving instance Data File

deriving instance Data Dir

deriving instance Typeable Abs

deriving instance Typeable Rel

deriving instance Typeable File

deriving instance Typeable Dir

deriving instance (Typeable a, Typeable b) => Typeable (Path a b)

deriving instance (Data a, Data b) => Data (Path a b)

deriving instance TH.Lift (Path a b)

deriving instance TH.Lift Boxed

deriving instance TH.Lift l => TH.Lift (Name l)

deriving instance TH.Lift l => TH.Lift (QName l)

deriving instance TH.Lift l => TH.Lift (ModuleName l)

deriving instance TH.Lift l => TH.Lift (SpecialCon l)

data InputSpec = InputSpec
    { inputSpecBaseDir :: Path Abs Dir
    , inputSpecFile :: Path Rel File
    } deriving (Show, Eq, Data, Typeable, TH.Lift)

inputSpecAbsFile :: InputSpec -> Path Abs File
inputSpecAbsFile InputSpec {..} = inputSpecBaseDir </> inputSpecFile

data SignatureInferenceStrategy = SignatureInferenceStrategy
    { sigInfStratName :: String
    , sigInfRelevantSources :: [Path Rel File]
    , inferSignature :: [EasyId] -> [EasyId] -> InferredSignature
    }

instance Show SignatureInferenceStrategy where
    show = sigInfStratName

instance Eq SignatureInferenceStrategy where
    s1 == s2 = sigInfStratName s1 == sigInfStratName s2

newtype InferredSignature =
    InferredSignature [([EasyNamedExp], Int, [Int])]
    deriving (Show, Eq)

prettyInferredSignature :: InferredSignature -> String
prettyInferredSignature (InferredSignature f) = ppShow f

newtype SignatureExpression =
    SignatureExpression EasyExp
    deriving (Show, Eq, Ord)

data Id m = Id
    { idName :: QName m
    , idType :: Type m
    , idImpl :: Maybe (Impl m)
    , idRootloc :: Maybe (Path Rel File) -- The module name where it was defined
    } deriving (Show, Eq, Ord, Generic)

type EasyId = Id ()

prettyEasyId :: EasyId -> String
prettyEasyId Id {..} =
    unwords [H.prettyPrint idName, "::", H.prettyPrint idType]

data NamedExp m = NamedExp
    { neName :: String
    , neExp :: Exp m
    } deriving (Show, Eq, Ord)

type EasyNamedExp = NamedExp ()

prettyEasyNameExp :: EasyNamedExp -> String
prettyEasyNameExp NamedExp {..} = unwords [neName, "=", prettyPrint neExp]

type EasyName = H.Name ()

type EasyQName = H.QName ()

type EasyType = H.Type ()

type EasyExp = H.Exp ()

type EasyStmt = H.Stmt ()

type EasyPat = H.Pat ()

data EasyEq =
    EasyEq EasyExp
           EasyExp
    deriving (Show, Eq, Ord, Generic)

prettyEasyEq :: EasyEq -> String
prettyEasyEq (EasyEq e1 e2) = unwords [H.prettyPrint e1, "=", H.prettyPrint e2]

newtype Impl l =
    Impl [Decl l]
    deriving (Show, Eq, Ord, Functor)

type EasyImpl = Impl ()

prettyEasyImpl :: EasyImpl -> String
prettyEasyImpl (Impl ds) = unlines $ map H.prettyPrint ds
