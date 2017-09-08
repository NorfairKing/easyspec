{-# LANGUAGE CPP #-}

module EasySpec.Discover.SignatureInference.TypeReachability where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.SignatureInference.ChunksPlusUtils
import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types
import EasySpec.Discover.Utils

{-# ANN module "HLint: ignore Use ||" #-}

inferChunksPlusTypeReachability :: Int -> SignatureInferenceStrategy
inferChunksPlusTypeReachability t =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus-type-reachability-" ++ show t
    , inferSignature = inferChunksPlusFrom $ depthNReachableViaComposition t
    }

inferChunksTypeReachability :: Int -> SignatureInferenceStrategy
inferChunksTypeReachability t =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-type-reachability-" ++ show t
    , inferSignature = inferChunksFrom $ depthNReachableViaComposition t
    }

inferTypeReachability :: Int -> SignatureInferenceStrategy
inferTypeReachability t =
    splitInferAlg ("type-reachability-" ++ show t) $
    depthNReachableViaComposition t

depthNReachableViaComposition :: Int -> [EasyId] -> [EasyId] -> [EasyId]
depthNReachableViaComposition = depthNReachable reachableViaComposition

depthNReachable ::
       (Show a, Ord a) => (a -> a -> Bool) -> Int -> [a] -> [a] -> [a]
depthNReachable _ 0 focus _ = focus
depthNReachable canReachFrom depth focus scope =
    let reachableSet =
            ordNub $
            concatMap
                    -- TODO use a better nub if needed
                (\ff -> filter (canReachFrom ff) (nub $ scope ++ focus))
                focus ++
            focus
    in depthNReachable canReachFrom (depth - 1) reachableSet scope

-- Symmetric
reachableViaComposition :: EasyId -> EasyId -> Bool
reachableViaComposition e1 e2 = typeReachable (idType e1) (idType e2)

typeReachable :: EasyType -> EasyType -> Bool
typeReachable t1 t2 =
    case t1 of
        TyFun _ t1l t1r ->
            or
                [ t1l == t2
                , t1r == t2
                , case t2 of
                      TyFun _ t2l t2r ->
                          or
                              [ t2l == t1 -- Left-hand side of second type equals the first type
                              , t2r == t1 -- Right-hand side of the second type equals the first type
                              , t1r == t2l -- Right-hand side of the first type equals the left-hand side of the second
                                           -- This means that t2 . t1 type-checks
                              , t1l == t2r -- Left-hand side of the first type equals the right-hand side of the second
                                           -- This means that t1 . t2 type-checks
                              ]
                      _ -> False
                ]
        _ -> False
