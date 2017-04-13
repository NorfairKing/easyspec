module EasySpec.Discover.Types where

import Import

import Language.Haskell.Exts.Syntax as H

data SignatureInferenceStrategy = SignatureInferenceStrategy
    { sigInfStratName :: String
    , inferSignature :: [EasyId] -> [EasyId] -> InferredSignature
    }

instance Show SignatureInferenceStrategy where
    show = sigInfStratName

instance Eq SignatureInferenceStrategy where
    s1 == s2 = sigInfStratName s1 == sigInfStratName s2

data InferredSignature = InferredSignature
    { sigFocusIds :: [EasyId]
    , sigBackgroundIds :: [EasyId]
    } deriving (Show, Eq)

newtype SignatureExpression =
    SignatureExpression EasyExp
    deriving (Show, Eq)

type EasyId = Id ()

data Id m = Id
    { idName :: Name m
    , idType :: Type m
    } deriving (Show, Eq)

type EasyName = H.Name ()

type EasyType = H.Type ()

type EasyExp = H.Exp ()

type EasyStmt = H.Stmt ()

type EasyPat = H.Pat ()

data EasyEq =
    EasyEq EasyExp
           EasyExp
    deriving (Show, Eq)
