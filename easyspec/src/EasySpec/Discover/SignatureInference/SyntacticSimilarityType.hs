{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.SyntacticSimilarityType where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityType :: Int -> SignatureInferenceStrategy
inferSyntacticSimilarityType i =
    similarityInferAlg
        ("syntactical-similarity-type-" ++ show i)
        [$(mkRelFile __FILE__)] i
        idSubTypes

idSubTypes :: EasyId -> [EasyType]
idSubTypes = getSubTypes . idType

getSubTypes :: Type l -> [Type l]
getSubTypes typ_ =
    case typ_ of
        TyForall l mtvbs btc t -> TyForall l mtvbs btc t : getSubTypes t
        TyFun l t1 t2 -> TyFun l t1 t2 : getSubTypes t1 ++ getSubTypes t2
        TyTuple l b ts -> TyTuple l b ts : concatMap getSubTypes ts
        TyList l lt -> TyList l lt : getSubTypes lt
        TyParArray l lt -> TyParArray l lt : getSubTypes lt
        TyApp l t1 t2 -> TyApp l t1 t2 : getSubTypes t1 ++ getSubTypes t2
        TyVar l n -> [TyVar l n]
        TyCon l qn -> [TyCon l qn]
        TyParen l t -> TyParen l t : getSubTypes t
        TyInfix l t1 qn t2 ->
            TyInfix l t1 qn t2 : getSubTypes t1 ++ getSubTypes t2
        TyKind l t k -> TyKind l t k : getSubTypes t
        TyPromoted l p -> [TyPromoted l p]
        TyEquals l t1 t2 -> TyEquals l t1 t2 : getSubTypes t1 ++ getSubTypes t2
        TySplice l spl -> [TySplice l spl]
        TyBang l bt up t -> TyBang l bt up t : getSubTypes t
        TyWildCard l mn -> [TyWildCard l mn]
        TyQuasiQuote l s1 s2 -> [TyQuasiQuote l s1 s2]
