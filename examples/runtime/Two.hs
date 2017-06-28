{-# LANGUAGE NoImplicitPrelude #-}

module Two where

import Prelude (reverse)

{-# ANN module "HLint: ignore Use foldr" #-}

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] bs = bs
myPlusPlus (a:as) bs = a : myPlusPlus as bs