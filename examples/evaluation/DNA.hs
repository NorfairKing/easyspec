{-# LANGUAGE NoImplicitPrelude #-}

module DNA where

import Data.Foldable (foldl')
import Data.List
       (sort, lookup, elem, splitAt, reverse, uncons, intersperse,length,
        intercalate, transpose, stripPrefix, group, inits, tails,
        isPrefixOf, isSuffixOf, isInfixOf, partition, nub, delete, (\\),
        union, intersect, sortOn, insert, isSubsequenceOf)
import Data.Maybe (isJust, isNothing)
import Prelude
       (Int, Num(..), Ord((<), (<=), (>), (>=), max, min), div, mod, odd, even, Bool(..), Integer,
        Float, Double, Rational, Word, Monoid(..), id, const, flip, ($),
        take, drop, splitAt, takeWhile, dropWhile, span, break, lookup,
        zip, zip3, zipWith, zipWith3, unzip, unzip3, scanl, scanl1, scanr,
        scanr1, map, (++), filter, Bounded(..), Enum(..), words, lines,
        unlines, unwords, Either(..), either, Ordering(..), Char, fst, snd,
        curry, uncurry, Maybe(..), maybe, (&&), (||), not, head, Eq(..))


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
