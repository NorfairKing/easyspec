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
    splitInferAlg name $ \focus scope ->
        take 5 $
        sortOn
            (\f -> sum $ map (\ff -> difference (dictOf ff) (dictOf f)) focus)
            scope
  where
    dictOf = letterDict . distil

letterDict :: (Hashable a, Eq a, Foldable f) => f a -> HashMap a Int
letterDict = foldl go HM.empty
  where
    go hm k = HM.alter u k hm
      where
        u Nothing = Just 1
        u (Just n) = Just (n + 1)

difference :: (Hashable a, Eq a) => HashMap a Int -> HashMap a Int -> Int
difference hm1 hm2 = HM.foldl' (+) 0 $ HM.unionWith go hm1 hm2
  where
    go :: Int -> Int -> Int
    go n1 n2 = abs (n1 - n2)
