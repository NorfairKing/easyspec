{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionSizeOfProperty
    ( dfrgSizeOfProperty
    ) where

import Import hiding (Alt)

import Language.Haskell.Exts.Syntax

import Data.Csv

import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Plots.DistributionFromRawPlotter
import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Types

{-# ANN module ("HLint: ignore Use const" :: String) #-}

dfrgSizeOfProperty :: DistributionFromRawGatherer Size
dfrgSizeOfProperty =
    DistributionFromRawGatherer
    { dfrgName = "size-of-property"
    , dfrgGatherFromPoints = sizesFromData
    , dfrgScript = scriptFile "size-of-property.r"
    }

sizesFromData :: [EvaluationInputPoint] -> [Size]
sizesFromData dats = map Size $ concatMap sizesFrom dats

sizesFrom :: EvaluationInputPoint -> [Int]
sizesFrom = map sizeOfEq . eipDiscoveredEqs

newtype Size =
    Size Int
    deriving (Show, Eq, Generic)

instance ToNamedRecord Size where
    toNamedRecord (Size i) = namedRecord ["size" .= i]

instance DefaultOrdered Size where
    headerOrder _ = header ["size"]

instance FromNamedRecord Size where
    parseNamedRecord r = Size <$> r .: "size"

sizeOfEq :: ES.EasyEq -> Int
sizeOfEq (ES.EasyEq t1 t2) = sizeOfExp t1 + sizeOfExp t2

sizeOfExp :: ES.EasyExp -> Int
sizeOfExp =
    ES.foldExp
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ b1 _ b2 -> 1 + b1 + b2)
        (\_ -> (+))
        (\_ -> (+ 1))
        (\_ ps b -> sum $ b : map sizeOfPat ps)
        (\_ bnds b -> sizeOfBinds bnds + b)
        (\_ b1 b2 b3 -> 1 + b1 + b2 + b3)
        (\_ grhs -> 1 + sum (map sizeOfGuardedRhs grhs))
        (\_ b as -> 1 + sum (b : map sizeOfAlt as))
        (\_ -> (+ 1) . sum . map sizeOfStmt)
        (\_ -> (+ 1) . sum . map sizeOfStmt)
        (\_ _ -> (+ 1) . sum)
        (\_ _ -> (+ 1) . sum . catMaybes)
        (\_ -> (+ 1) . sum)
        (\_ -> (+ 1) . sum)
        (\_ -> id)
        (\_ b _ -> 1 + b)
        (\_ _ b -> 1 + b)
        (\_ _ fus -> 1 + sum (map sizeOfFieldUpdate fus))
        (\_ b fus -> 1 + b + sum (map sizeOfFieldUpdate fus))
        (\_ b -> 1 + b)
        (\_ b b' -> 1 + b + b')
        (\_ b b' -> 1 + b + b')
        (\_ b b' b'' -> 1 + b + b' + b'')
        (\_ b b' -> 1 + b + b')
        (\_ b b' b'' -> 1 + b + b' + b'')
        (\_ b qs -> 1 + b + sum (map sizeOfQualStmt qs))
        (\_ b qss -> 1 + b + sum (concatMap (map sizeOfQualStmt) qss))
        (\_ b qss -> 1 + b + sum (concatMap (map sizeOfQualStmt) qss))
        (\_ b _ -> b)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ -> 1)
        (\_ _ _ -> 1)
        (\_ _ -> 1)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)
        (\_ -> undefined)

sizeOfPat :: Pat l -> Int
sizeOfPat = undefined

sizeOfBinds :: Binds l -> Int
sizeOfBinds = undefined

sizeOfGuardedRhs :: GuardedRhs l -> Int
sizeOfGuardedRhs = undefined

sizeOfStmt :: Stmt l -> Int
sizeOfStmt = undefined

sizeOfAlt :: Alt l -> Int
sizeOfAlt = undefined

sizeOfQualStmt :: QualStmt l -> Int
sizeOfQualStmt = undefined

sizeOfFieldUpdate :: FieldUpdate l -> Int
sizeOfFieldUpdate = undefined
