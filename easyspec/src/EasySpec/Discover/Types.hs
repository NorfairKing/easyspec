module EasySpec.Discover.Types where

import Import

import Language.Haskell.Exts.Syntax as H

type EasyId = Id ()

data Id m = Id
    { idName :: Name m
    , idType :: Type m
    } deriving( Show, Eq)

type EasyName = H.Name ()

type EasyType = H.Type ()

type EasyExp = H.Exp ()
