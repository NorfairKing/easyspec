{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.SyntacticSimilarityName where

import Import

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityName :: Int -> SignatureInferenceStrategy
inferSyntacticSimilarityName i =
    similarityInferAlg
        ("syntactical-similarity-name-" ++ show i)
        [$(mkRelFile __FILE__)]
        i
        (prettyPrintOneLine . idName)
