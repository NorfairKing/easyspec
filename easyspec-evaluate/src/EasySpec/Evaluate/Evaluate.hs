{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module EasySpec.Evaluate.Evaluate where

import Import

import System.TimeIt
import Text.Printf

import Data.Csv

import qualified Data.ByteString.Lazy.Char8 as LB8

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse as ES
import qualified EasySpec.OptParse.Types as ES

import EasySpec.Evaluate.Types

runEvaluate :: [ES.InputSpec] -> IO ()
runEvaluate iss = do
    epointss <- mapM getEvaluationInputPointsFor iss
    LB8.putStrLn $
        encodeDefaultOrderedByName $
        concatMap evaluationInputPointCsvLines $ concat epointss

namesInSource
    :: MonadIO m
    => ES.InputSpec -> m [ES.EasyName]
namesInSource is =
    map ES.idName <$> runReaderT (ES.getEasyIds is) evaluationSettings

getEvaluationInputPointsFor :: ES.InputSpec -> IO [EvaluationInputPoint]
getEvaluationInputPointsFor is = do
    names <- namesInSource is
    fmap concat $ forM names $ getEvaluationInputPointsForName is

getEvaluationInputPointsForName :: ES.InputSpec
                                -> ES.EasyName
                                -> IO [EvaluationInputPoint]
getEvaluationInputPointsForName is funcname =
    forM ES.inferenceStrategies $ getEvaluationInputPoint is funcname

evaluationSettings :: ES.Settings
evaluationSettings = ES.Settings {ES.setsDebugLevel = 0}

getEvaluationInputPoint
    :: ES.InputSpec
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> IO EvaluationInputPoint
getEvaluationInputPoint is funcname strat = do
    let ds =
            ES.DiscoverSettings
            { ES.setDiscInputSpec = is
            , ES.setDiscFun = Just funcname
            , ES.setDiscInfStrat = strat
            }
    (runtime, eqs) <-
        timeItT $ runReaderT (ES.discoverEquations ds) evaluationSettings
    pure
        EvaluationInputPoint
        { eipInputSpec = is
        , eipFunc = funcname
        , eipStrat = strat
        , eipDiscoveredEqs = eqs
        , eipRuntime = runtime
        }

evaluators :: [Evaluator]
evaluators =
    [ equationsEvaluator
    , runtimeEvaluator
    , relevantEquationsEvaluator
    , irrelevantEquationsEvaluator
    , relativeRelevantEquationsEvaluator
    ]

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
            , ES.sigInfStratName $ eipStrat eip
            , prettyPrint $ eipFunc eip
            , evaluatorName ev
            , evaluate ip ev
            ]

evaluationInputPointCsvLines :: EvaluationInputPoint -> [EvaluatorCsvLine]
evaluationInputPointCsvLines eip = map line evaluators
  where
    ip = pointToInput eip
    line ev =
        EvaluatorCsvLine
        { eclBaseDir = ES.inputSpecBaseDir $ eipInputSpec eip
        , eclFile = ES.inputSpecFile $ eipInputSpec eip
        , eclStratName = ES.sigInfStratName $ eipStrat eip
        , eclFocusFuncName = prettyPrint $ eipFunc eip
        , eclEvaluatorName = evaluatorName ev
        , eclEvaluatorOutput = evaluatorGather ev ip
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

evaluate :: EvaluationInput -> Evaluator -> String
evaluate ei e = evaluatorPretty e $ evaluatorGather e ei

pointToInput :: EvaluationInputPoint -> EvaluationInput
pointToInput EvaluationInputPoint {..} =
    EvaluationInput
    { eiDiscoveredEqs = eipDiscoveredEqs
    , eiRuntime = eipRuntime
    , eiFocusFuncName = eipFunc
    }

equationsEvaluator :: Evaluator
equationsEvaluator =
    Evaluator "equations" (genericLength . eiDiscoveredEqs) show

runtimeEvaluator :: Evaluator
runtimeEvaluator = Evaluator "runtime" eiRuntime (printf "%.3f")

relativeRelevantEquationsEvaluator :: Evaluator
relativeRelevantEquationsEvaluator =
    Evaluator "relative-relevant-equations" go (printf "%.2f")
  where
    go ei =
        let l =
                evaluatorGather relevantEquationsEvaluator ei /
                evaluatorGather equationsEvaluator ei
        in if isNaN l
               then 0
               else l

relevantEquationsEvaluator :: Evaluator
relevantEquationsEvaluator = Evaluator "relevant-equations" go show
  where
    go ei =
        genericLength $
        filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)

irrelevantEquationsEvaluator :: Evaluator
irrelevantEquationsEvaluator = Evaluator "irrelevant-equations" go show
  where
    go ei =
        genericLength $
        filter (not . mentionsEq (eiFocusFuncName ei)) (eiDiscoveredEqs ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2
