{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate where

import Import

import System.TimeIt
import Text.Printf

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse as ES

import EasySpec.Evaluate.OptParse
import EasySpec.Evaluate.Types

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    (DispatchEvaluate fs, Settings) <- getInstructions
    epointss <- mapM getEvaluationInputPointsFor fs
    putStrLn $ showEvaluationReport epointss

getEvaluationInputPointsFor :: Path Abs File -> IO [EvaluationInputPoint]
getEvaluationInputPointsFor f = do
    eids <- runReaderT (ES.getEasyIds f) evaluationSettings
    fmap concat $ forM (map ES.idName eids) $ getEvaluationInputPointsForName f

getEvaluationInputPointsForName ::
       Path Abs File -> ES.EasyName -> IO [EvaluationInputPoint]
getEvaluationInputPointsForName f funcname =
    forM ES.inferenceStrategies $ getEvaluationInputPoint f funcname

evaluationSettings :: ES.Settings
evaluationSettings = ES.Settings {ES.setsDebugLevel = 0}

getEvaluationInputPoint ::
       Path Abs File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> IO EvaluationInputPoint
getEvaluationInputPoint f funcname strat = do
    let ds =
            ES.DiscoverSettings
            { ES.setDiscFile = f
            , ES.setDiscFun = Just funcname
            , ES.setDiscInfStrat = strat
            }
    (runtime, eqs) <-
        timeItT $ runReaderT (ES.discoverEquations ds) evaluationSettings
    pure
        EvaluationInputPoint
        { eipFile = f
        , eipFunc = funcname
        , eipStrat = strat
        , eipDiscoveredEqs = eqs
        , eipRuntime = runtime
        }

showEvaluationReport :: [[EvaluationInputPoint]] -> String
showEvaluationReport pointss = showTable $ concatMap go $ concat pointss
  where
    go :: EvaluationInputPoint -> [[String]]
    go eip =
        [ line lengthEvaluator
        , line runtimeEvaluator
        , line relevantEquationsEvaluator
        , line irrelevantEquationsEvaluator
        , line relativeRelevantEquationsEvaluator
        ]
      where
        ip = pointToInput eip
        line ev =
            [ toFilePath $ eipFile eip
            , ES.sigInfStratName $ eipStrat eip
            , prettyPrint $ eipFunc eip
            , evaluatorName ev
            , evaluate ip ev
            ]

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

evaluate :: EvaluationInput -> Evaluator a -> String
evaluate ei e = evaluatorPretty e $ evaluatorGather e ei

pointToInput :: EvaluationInputPoint -> EvaluationInput
pointToInput EvaluationInputPoint {..} =
    EvaluationInput
    { eiDiscoveredEqs = eipDiscoveredEqs
    , eiRuntime = eipRuntime
    , eiFocusFuncName = eipFunc
    }

lengthEvaluator :: Evaluator Int
lengthEvaluator = Evaluator "length" (length . eiDiscoveredEqs) show

runtimeEvaluator :: Evaluator Double
runtimeEvaluator = Evaluator "runtime" eiRuntime (printf "%.2fs")

relativeRelevantEquationsEvaluator :: Evaluator Double
relativeRelevantEquationsEvaluator =
    Evaluator "relative-relevant-equations" go (printf "%.2f")
  where
    go ei =
        genericLength
            (filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)) /
        genericLength (eiDiscoveredEqs ei)

relevantEquationsEvaluator :: Evaluator Int
relevantEquationsEvaluator = Evaluator "relevant-equations" go show
  where
    go ei =
        length $ filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)

irrelevantEquationsEvaluator :: Evaluator Int
irrelevantEquationsEvaluator = Evaluator "irrelevant-equations" go show
  where
    go ei =
        length $
        filter (not . mentionsEq (eiFocusFuncName ei)) (eiDiscoveredEqs ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2
