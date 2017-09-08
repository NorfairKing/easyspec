{-# LANGUAGE CPP #-}

module EasySpec.Discover.SignatureInference.SyntacticSimilarityName where

import Import

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityName :: Int -> SignatureInferenceStrategy
inferSyntacticSimilarityName i =
    similarityInferAlg ("syntactical-similarity-name-" ++ show i) i idNameStr

simDiffName :: EasyId -> EasyId -> Int
simDiffName = simDiff idNameStr

idNameStr :: EasyId -> String
idNameStr = prettyPrintOneLine . idName
