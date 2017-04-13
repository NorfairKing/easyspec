{-# LANGUAGE RecordWildCards #-}

module EasySpec.Evaluate where

import Import

import System.TimeIt
import Text.Printf

import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse as ES

import EasySpec.Evaluate.OptParse
import EasySpec.Evaluate.Types

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    (DispatchEvaluate exdir, Settings) <- getInstructions
    fs <- (filter ((== ".hs") . fileExtension) . snd) <$> listDirRecur exdir
    epointss <- mapM getEvaluationInputPointsFor fs
    putStrLn $ showEvaluationReport epointss

getEvaluationInputPointsFor :: Path Abs File -> IO [EvaluationInputPoint]
getEvaluationInputPointsFor f = do
    eids <- ES.getEasyIds f
    fmap concat $
        forM (map ES.idName eids) $ \funcname ->
            forM ES.inferenceStrategies $ getEvaluationInputPoint f funcname

getEvaluationInputPoint ::
       Path Abs File
    -> ES.EasyName
    -> ES.SignatureInferenceStrategy
    -> IO EvaluationInputPoint
getEvaluationInputPoint f funcname strat = do
    let sets = ES.Settings
        ds =
            ES.DiscoverSettings
            { ES.setDiscFile = f
            , ES.setDiscFun = Just funcname
            , ES.setDiscInfStrat = strat
            }
    (runtime, eqs) <- timeItT $ runReaderT (ES.discoverEquations ds) sets
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
    go eip = [line lengthEvaluator, line runtimeEvaluator]
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
    EvaluationInput {eiDiscoveredEqs = eipDiscoveredEqs, eiRuntime = eipRuntime}

lengthEvaluator :: Evaluator Int
lengthEvaluator = Evaluator "length" (length . eiDiscoveredEqs) show

runtimeEvaluator :: Evaluator Double
runtimeEvaluator = Evaluator "runtime" eiRuntime (printf "%.2fs")
