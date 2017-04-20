module EasySpec.Discover.SignatureInference.SyntacticSimilarityName where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

import EasySpec.Discover.CodeUtils
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferSyntacticSimilarityName :: SignatureInferenceStrategy
inferSyntacticSimilarityName =
    splitInferAlg "syntactically-similarity-name" $ \focus scope ->
        take 5 $
        sortOn
            (\f -> sum $ map (\ff -> difference (dictOf ff) (dictOf f)) focus)
            scope
  where
    dictOf :: EasyId -> HashMap Char Int
    dictOf = letterDict . prettyPrintOneLine . idName

letterDict :: String -> HashMap Char Int
letterDict = foldl go HM.empty
  where
    go :: HashMap Char Int -> Char -> HashMap Char Int
    go hm k = HM.alter u k hm
      where
        u Nothing = Just 1
        u (Just n) = Just (n + 1)

difference :: HashMap Char Int -> HashMap Char Int -> Int
difference hm1 hm2 = HM.foldl' (+) 0 $ HM.unionWith go hm1 hm2
  where
    go :: Int -> Int -> Int
    go n1 n2 = abs (n1 - n2)
