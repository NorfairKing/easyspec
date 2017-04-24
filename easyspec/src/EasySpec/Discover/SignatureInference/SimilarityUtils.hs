module EasySpec.Discover.SignatureInference.SimilarityUtils where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

-- Make a signature inference strategy, by describing how to get a 'fingerprint'
-- from an 'EasyId'.
similarityInferAlg ::
       (Hashable a, Eq a, Foldable f)
    => String
    -> (EasyId -> f a)
    -> SignatureInferenceStrategy
similarityInferAlg name distil =
    differenceInferAlg name $ \e1 e2 -> dictDiff (dictOf e1) (dictOf e2)
  where
    dictOf = letterDict . distil

-- Make a signature inference strategy, by describing the difference between two 'EasyId's.
differenceInferAlg ::
       (Ord n, Num n) => String -> (EasyId -> EasyId -> n) -> SignatureInferenceStrategy
differenceInferAlg name diff =
    splitInferAlg name $ \focus scope ->
        take 5 $ sortOn (\f -> sum $ map (\ff -> diff ff f) focus) scope

letterDict :: (Hashable a, Eq a, Foldable f) => f a -> HashMap a Int
letterDict = foldl go HM.empty
  where
    go hm k = HM.alter u k hm
      where
        u Nothing = Just 1
        u (Just n) = Just (n + 1)

dictDiff :: (Hashable a, Eq a) => HashMap a Int -> HashMap a Int -> Int
dictDiff hm1 hm2 = HM.foldl' (+) 0 $ HM.unionWith go hm1 hm2
  where
    go :: Int -> Int -> Int
    go n1 n2 = abs (n1 - n2)
