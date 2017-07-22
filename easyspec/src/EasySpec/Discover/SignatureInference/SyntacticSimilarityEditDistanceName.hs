{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.SyntacticSimilarityEditDistanceName where

import Import

import qualified Data.Vector as V
import Data.Vector.Distance

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.Types

inferSyntacticSimilarityEditDistanceName :: Int -> SignatureInferenceStrategy
inferSyntacticSimilarityEditDistanceName i =
    differenceInferAlg
        ("syntactical-similarity-edit-distance-name" ++ show i)
        [$(mkRelFile __FILE__)]
        i $ \i1 i2 ->
        let (Sum c, _) = leastChanges strParams (idVec i1) (idVec i2)
        in c
  where
    idVec = V.fromList . prettyPrintOneLine . idName
