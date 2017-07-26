{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module EasySpec.Evaluate.Evaluate where

import Import

import System.TimeIt

import Data.Csv

import qualified Data.ByteString.Lazy.Char8 as LB8

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse as ES
import qualified EasySpec.OptParse.Types as ES

import EasySpec.Evaluate.Analyse.Data.Common
import EasySpec.Evaluate.Evaluate.Evaluator
import EasySpec.Evaluate.Evaluate.Evaluator.Types
import EasySpec.Evaluate.Types

runEvaluate :: [ES.InputSpec] -> IO ()
runEvaluate iss = do
    epointss <- mapM getEvaluationInputPointsFor iss
    let csvLines =
            [ evaluationInputPointCsvLine p e
            | e <- evaluators
            , p <- concat epointss
            ]
    LB8.putStrLn $ encodeDefaultOrderedByName csvLines

getEvaluationInputPointsFor :: ES.InputSpec -> IO [EvaluationInputPoint]
getEvaluationInputPointsFor is = do
    names <- namesInSource is
    fmap concat $ forM names $ getEvaluationInputPointsForName is

getEvaluationInputPointsForName ::
       ES.InputSpec -> ES.EasyQName -> IO [EvaluationInputPoint]
getEvaluationInputPointsForName is funcname =
    forM ES.inferenceStrategies $ getEvaluationInputPoint is funcname

evaluationSettings :: ES.Settings
evaluationSettings = ES.Settings {ES.setsDebugLevel = 0}

getEvaluationInputPoint ::
       ES.InputSpec
    -> ES.EasyQName
    -> ES.SignatureInferenceStrategy
    -> IO EvaluationInputPoint
getEvaluationInputPoint is funcname strat = do
    let ds =
            ES.DiscoverSettings
            { ES.setDiscInputSpec = is
            , ES.setDiscFun = Just funcname
            , ES.setDiscInfStrat = strat
            , ES.setDiscQualified = ES.UnqualifyNothing
            }
    ids <- runReaderT (ES.getEasyIds is) evaluationSettings
    (runtime, eqs) <-
        timeItT $ runReaderT (ES.discoverEquations ds) evaluationSettings
    pure
        EvaluationInputPoint
        { eipInputSpec = is
        , eipStrat = ES.sigInfStratName strat
        , eipFunc = funcname
        , eipScope = ids
        , eipDiscoveredEqs = eqs
        , eipRuntime = runtime
        }

showEvaluationReport :: [[EvaluationInputPoint]] -> String
showEvaluationReport pointss = showTable $ concatMap go $ concat pointss
  where
    go :: EvaluationInputPoint -> [[String]]
    go eip = map line evaluators
      where
        ip = pointToInput eip
        line ev =
            [ toFilePath $ ES.inputSpecBaseDir $ eipInputSpec eip
            , toFilePath $ ES.inputSpecFile $ eipInputSpec eip
            , eipStrat eip
            , prettyPrint $ eipFunc eip
            , evaluatorName ev
            , evaluatorPretty ev ip
            ]

evaluationInputPointCsvLine ::
       EvaluationInputPoint -> Evaluator -> EvaluatorCsvLine
evaluationInputPointCsvLine eip ev = line
  where
    ip = pointToInput eip
    line =
        EvaluatorCsvLine
        { eclBaseDir = ES.inputSpecBaseDir $ eipInputSpec eip
        , eclFile = ES.inputSpecFile $ eipInputSpec eip
        , eclStratName = eipStrat eip
        , eclFocusFuncName = prettyPrint $ eipFunc eip
        , eclEvaluatorName = evaluatorName ev
        , eclEvaluatorOutput = evaluatorGather ev ip
        , eclEvaluatorUnit = evaluatorUnit ev
        , eclEvaluatorQuantity = evaluatorQuantity ev
        , eclEvaluatorPrettyOutput = evaluatorPretty ev ip
        }

showTable :: [[String]] -> String
showTable = unlines . map unwords . formatTable

formatTable :: [[String]] -> [[String]]
formatTable sss =
    transpose $
    flip map (transpose sss) $ \ss ->
        let padL = maximum $ map length ss
        in map (pad ' ' $ padL + 1) ss

pad :: Char -> Int -> String -> String
pad c i s
    | length s < i = s ++ replicate (i - length s) c
    | otherwise = s

pointToInput :: EvaluationInputPoint -> EvaluationInput
pointToInput EvaluationInputPoint {..} =
    EvaluationInput
    { eiScope = eipScope
    , eiFocusFuncName = eipFunc
    , eiDiscoveredEqs = eipDiscoveredEqs
    , eiRuntime = eipRuntime
    }
