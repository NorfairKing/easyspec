{-# LANGUAGE NoImplicitPrelude #-}

module Two where

{-# ANN module "HLint: ignore Use foldr" #-}

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] bs = bs
myPlusPlus (a:as) bs = a : myPlusPlus as bs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as `myPlusPlus` [a]
