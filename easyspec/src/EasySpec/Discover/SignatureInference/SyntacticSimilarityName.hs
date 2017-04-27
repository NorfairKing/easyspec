module EasySpec.Discover.SignatureInference.SyntacticSimilarityName where

import Import

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityName :: SignatureInferenceStrategy
inferSyntacticSimilarityName =
    similarityInferAlg
        "syntactical-similarity-name"
        (prettyPrintOneLine . idName)
