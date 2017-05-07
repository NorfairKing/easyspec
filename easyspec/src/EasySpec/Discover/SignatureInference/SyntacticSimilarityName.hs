module EasySpec.Discover.SignatureInference.SyntacticSimilarityName where

import Import

import qualified Data.Vector as V
import Data.Vector.Distance

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityName :: SignatureInferenceStrategy
inferSyntacticSimilarityName =
    similarityInferAlg
        "syntactical-similarity-name"
        (prettyPrintOneLine . idName)

inferSyntacticSimilarityEditDistanceName :: SignatureInferenceStrategy
inferSyntacticSimilarityEditDistanceName =
    differenceInferAlg "syntactical-similarity-edit-distance-name" $ \i1 i2 ->
        let (Sum c, _) = leastChanges strParams (idVec i1) (idVec i2)
        in c
  where
    idVec = V.fromList . prettyPrintOneLine . idName
