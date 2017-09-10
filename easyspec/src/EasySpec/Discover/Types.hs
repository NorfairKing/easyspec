{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
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
    , inputSpecFile :: !(Path Abs File)
    } deriving (Show, Eq, Data, Typeable, TH.Lift)

data SignatureInferenceStrategy = SignatureInferenceStrategy
    { sigInfStratName :: String
    , inferSignature :: [EasyId] -> [EasyId] -> InferredSignature
    }

instance Show SignatureInferenceStrategy where
    show = sigInfStratName

instance Eq SignatureInferenceStrategy where
    s1 == s2 = sigInfStratName s1 == sigInfStratName s2

newtype InferredSignature =
    InferredSignature (InferM ())

data InferM a where
    InferPure :: a -> InferM a
    InferFmap :: (a -> b) -> InferM a -> InferM b
    InferApp :: InferM (a -> b) -> InferM a -> InferM b
    InferBind :: InferM a -> (a -> InferM b) -> InferM b
    -- And this is the special one:
    InferFrom :: [EasyNamedExp] -> [OptiToken] -> InferM (OptiToken, [EasyEq])

inferFromWith :: [EasyNamedExp] -> [OptiToken] -> InferM (OptiToken, [EasyEq])
inferFromWith = InferFrom

inferFrom :: [EasyNamedExp] -> InferM (OptiToken, [EasyEq])
inferFrom = (`InferFrom` [])

inferFrom_ :: [EasyNamedExp] -> InferM OptiToken
inferFrom_ = InferFmap fst . (`InferFrom` [])

inferFrom__ :: [EasyNamedExp] -> InferM ()
inferFrom__ = void . (`InferFrom` [])

newtype OptiToken =
    OptiToken Int

instance Functor InferM where
    fmap = InferFmap

instance Applicative InferM where
    pure = InferPure
    (<*>) = InferApp

instance Monad InferM where
    return = pure
    (>>=) = InferBind

newtype SignatureExpression =
    SignatureExpression EasyExp
    deriving (Show, Eq, Ord)

data Id m = Id
    { idName :: QName m
    , idType :: Type m
    , idImpl :: Maybe (Impl m)
    , idRootloc :: !(Maybe (Path Abs File)) -- The module name where it was defined
    } deriving (Show, Eq, Ord, Generic)

type EasyId = Id ()

prettyEasyId :: EasyId -> String
prettyEasyId Id {..} =
    unwords [H.prettyPrint idName, "::", H.prettyPrint idType]

data NamedExp m = NamedExp
    { neName :: QName m
    , neExp :: Exp m
    } deriving (Show, Eq, Ord)

type EasyNamedExp = NamedExp ()

prettyEasyNameExp :: EasyNamedExp -> String
prettyEasyNameExp NamedExp {..} =
    unwords [prettyPrint neName, "=", prettyPrint neExp]

type EasyName = H.Name ()

type EasyModuleName = H.ModuleName ()

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
