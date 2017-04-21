{-# LANGUAGE RecordWildCards #-}

module EasySpec.Discover.Types where

import Import

import Data.Tree

import Language.Haskell.Exts.Pretty as H
import Language.Haskell.Exts.Syntax as H

data SignatureInferenceStrategy = SignatureInferenceStrategy
    { sigInfStratName :: String
    , inferSignature :: [EasyId] -> [EasyId] -> InferredSignature
    }

instance Show SignatureInferenceStrategy where
    show = sigInfStratName

instance Eq SignatureInferenceStrategy where
    s1 == s2 = sigInfStratName s1 == sigInfStratName s2

newtype InferredSignature =
    InferredSignature (Tree [EasyNamedExp])
    deriving (Show, Eq)

newtype SignatureExpression =
    SignatureExpression EasyExp
    deriving (Show, Eq)

data Id m = Id
    { idName :: Name m
    , idType :: Type m
    , idImpl :: Maybe (Impl m)
    } deriving (Show, Eq)

type EasyId = Id ()

prettyEasyId :: EasyId -> String
prettyEasyId Id {..} =
    unwords [H.prettyPrint idName, "::", H.prettyPrint idType]

data NamedExp m = NamedExp
    { neName :: String
    , neExp :: Exp m
    } deriving (Show, Eq)

type EasyNamedExp = NamedExp ()

type EasyName = H.Name ()

type EasyType = H.Type ()

type EasyExp = H.Exp ()

type EasyStmt = H.Stmt ()

type EasyPat = H.Pat ()

data EasyEq =
    EasyEq EasyExp
           EasyExp
    deriving (Show, Eq)

prettyEasyEq :: EasyEq -> String
prettyEasyEq (EasyEq e1 e2) = unwords [H.prettyPrint e1, "=", H.prettyPrint e2]

newtype Impl l =
    Impl [H.Match l]
    deriving (Show, Eq)

type EasyImpl = Impl ()
