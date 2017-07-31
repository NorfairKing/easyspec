{-# LANGUAGE CPP #-}

module EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilaritySymbols :: Int -> SignatureInferenceStrategy
inferSyntacticSimilaritySymbols i =
    differenceInferAlg
        ("syntactical-similarity-symbols-" ++ show i)
        i
        diffIdImpl

diffIdImpl :: EasyId -> EasyId -> Double
diffIdImpl i1 i2 =
    case (idImpl i1, idImpl i2) of
        (Just impl1, Just impl2) -> diffImpl impl1 impl2
        _ -> read "Infinity" -- FIXME, this is rather ugly

diffImpl :: EasyImpl -> EasyImpl -> Double
diffImpl ei1 ei2 =
    fromIntegral $
    dictDiff (letterDict $ namesOf ei1) (letterDict $ namesOf ei2)

namesOf :: EasyImpl -> [String]
namesOf (Impl d) =
    map nameStr $ concatMap getQNameSymbols $ concatMap getDeclSymbols d
  where
    nameStr :: Name () -> String
    nameStr (Ident _ s) = s
    nameStr (Symbol _ s) = s
