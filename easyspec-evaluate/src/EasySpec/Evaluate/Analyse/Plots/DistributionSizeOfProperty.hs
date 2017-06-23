{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EasySpec.Evaluate.Analyse.Plots.DistributionSizeOfProperty where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Csv

import Development.Shake

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.Discover.Utils as ES

import EasySpec.Evaluate.Analyse.Plots.Files
import EasySpec.Evaluate.Analyse.Plots.ResultsPlots
import EasySpec.Evaluate.Types

plotsRulesDistributionDistributionSizeOfProperty :: Rules [Path Abs File]
plotsRulesDistributionDistributionSizeOfProperty = do
    plotF <- scriptFile "size-of-property.r"
    resultsPlotsFor
        EvaluationFunc
        { evaluationFuncDir = $(mkRelDir "size-of-property")
        , evaluationFuncEval = sizesFromData
        , evaluationFuncIndividualMessage =
              \e n s csvF ->
                  unwords
                      [ "Calculating the size of properties in the results of running easyspec on"
                      , toFilePath $ ES.inputSpecAbsFile e
                      , "with focus"
                      , show $ prettyPrint n
                      , "with signature inference strategy"
                      , show $ ES.sigInfStratName s
                      , "and writing the results to"
                      , toFilePath csvF
                      ]
        , evaluationFuncPerStrategyMessage =
              \s csvF ->
                  unwords
                      [ "Calculating the size of properties in the results of running easyspec on all examples and names, but with signature inference strategy"
                      , show $ ES.sigInfStratName s
                      , "and writing the results to"
                      , toFilePath csvF
                      ]
        , evaluationFuncPlotScript = plotF
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
sizeOfExp = undefined
