module EasySpec.Discover.SignatureInference.SyntacticSimilaritySymbols where

import Import

import Language.Haskell.Exts.Syntax

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilaritySymbols :: SignatureInferenceStrategy
inferSyntacticSimilaritySymbols =
    differenceInferAlg "syntactically-similarity-symbols" diffIdImpl

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
namesOf (Impl ms) = map nameStr $ concatMap go ms
  where
    go :: Match l -> [Name l]
    go (Match _ n ps rhs mbs) = n : concatMap getPatSymbols ps
    go (InfixMatch _ p1 n ps rhs mbs) =
        n : getPatSymbols p1 ++ concatMap getPatSymbols ps
    nameStr :: Name () -> String
    nameStr (Ident _ s) = s
    nameStr (Symbol _ s) = s
