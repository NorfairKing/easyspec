{-# LANGUAGE NoImplicitPrelude #-}

module Three where

import Prelude (id, reverse)

{-# ANN module "HLint: ignore Use foldr" #-}

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] bs = bs
myPlusPlus (a:as) bs = a : myPlusPlus as bs
