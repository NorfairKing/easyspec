{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.SimilarityUtils where

import Import

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

-- Make a signature inference strategy, by describing how to get a 'fingerprint'
-- from an 'EasyId'.
similarityInferAlg
    :: (Eq a, Ord a, Foldable f)
    => String
    -> [Path Rel File]
    -> (EasyId -> f a)
    -> SignatureInferenceStrategy
similarityInferAlg name fs distil =
    differenceInferAlg name ($(mkRelFile __FILE__) : fs) $ \e1 e2 ->
        dictDiff (dictOf e1) (dictOf e2)
  where
    dictOf = letterDict . distil

-- Make a signature inference strategy, by describing the difference between two 'EasyId's.
differenceInferAlg
    :: (Ord n, Show n, Num n)
    => String
    -> [Path Rel File]
    -> (EasyId -> EasyId -> n)
    -> SignatureInferenceStrategy
differenceInferAlg name fs diff =
    splitInferAlg name ($(mkRelFile __FILE__) : fs) $ \focus scope ->
        take 5 $ sortOn (\f -> sum $ map (diff f) focus) scope

letterDict
    :: (Eq a, Ord a, Foldable f)
    => f a -> Map a Int
letterDict = foldl' go M.empty
  where
    go hm k = M.alter u k hm
      where
        u Nothing = Just 1
        u (Just n) = Just (n + 1)

dictDiff
    :: (Eq a, Ord a)
    => Map a Int -> Map a Int -> Int
dictDiff hm1 hm2 = M.foldl' (+) 0 $ M.unionWith go hm1 hm2
  where
    go :: Int -> Int -> Int
    go n1 n2 = abs (n1 - n2)
