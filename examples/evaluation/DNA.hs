{-# LANGUAGE NoImplicitPrelude #-}

module DNA where

import Data.Foldable (foldl')
import Data.List (sort, lookup, elem, splitAt, reverse)
import Prelude
       (Int, Char, (/), (+), Maybe(..), fst, filter, (==), (/=), map,
        take, drop, Double, Bool(..), zip, fromIntegral, length)

{-# ANN module "HLint: ignore Use String" #-}

{-# ANN module "HLint: ignore Use uncurry" #-}

onlyDNA :: [Char] -> [Char]
onlyDNA = filter (`elem` "TACG")

count :: [Char] -> [(Char, Int)]
count = foldl' go []
  where
    go :: [(Char, Int)] -> Char -> [(Char, Int)]
    go ls c =
        let rest = filter (\(d, _) -> d /= c) ls
            e =
                ( c
                , case lookup c ls of
                      Nothing -> 1
                      Just i -> i + 1)
        in sort (e : rest)

toRNA :: [Char] -> [Char]
toRNA = map go
  where
    go 'T' = 'U'
    go c = c

onlyRNA :: [Char] -> [Char]
onlyRNA ls = toRNA (onlyDNA ls)

complement :: [Char] -> [Char]
complement = map go
  where
    go 'A' = 'T'
    go 'T' = 'A'
    go 'C' = 'G'
    go 'G' = 'C'
    go c = c

reverseComplement :: [Char] -> [Char]
reverseComplement ls = complement (reverse ls)

gcCount :: [Char] -> Int
gcCount ls = length (filter (`elem` "GC") ls)

hamming :: [Char] -> [Char] -> Int
hamming as bs = length (filter (\(a, b) -> a /= b) (zip as bs))

reversePalindrome :: [Char] -> Bool
reversePalindrome ls = ls == reverseComplement ls
