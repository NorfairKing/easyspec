module EasySpec.Discover.SignatureInference.Monomorphisation where

import Import

import Language.Haskell.Exts.Syntax

-- import EasySpec.Discover.CodeUtils
import EasySpec.Discover.Types

-- All types and subtypes
findAllTypesAndSubtypes :: [EasyType] -> [EasyType]
findAllTypesAndSubtypes = undefined

kindOf :: Type l -> Kind l
kindOf = undefined

monomorphise ::
       [Type l] -- Types available for monomorphisation
    -> Type l -- Type to monomorphise
    -> Type l
monomorphise = undefined
