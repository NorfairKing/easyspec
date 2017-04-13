module EasySpec.Evaluate where

import Import

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
getEvaluationInputPointsFor f =
    forM ES.inferenceStrategies $ getEvaluationInputPoint f

getEvaluationInputPoint ::
       Path Abs File -> ES.SignatureInferenceStrategy -> IO EvaluationInputPoint
getEvaluationInputPoint f strat = do
    let sets = ES.Settings
        ds =
            ES.DiscoverSettings
            { ES.setDiscFile = f
            , ES.setDiscFun = Nothing
            , ES.setDiscInfStrat = strat
            }
    eqs <- runReaderT (ES.discoverEquations ds) sets
    pure
        EvaluationInputPoint
        {eipFile = f, eipStrat = strat, eipDiscoveredEqs = eqs}

showEvaluationReport :: [[EvaluationInputPoint]] -> String
showEvaluationReport pointss = unlines $ concatMap go $ concat pointss
  where
    go :: EvaluationInputPoint -> [String]
    go eip =
        [ unwords
              [ toFilePath $ eipFile eip
              , ES.sigInfStratName $ eipStrat eip
              , evaluatorName lengthEvaluator
              , evaluate (pointToInput eip) lengthEvaluator
              ]
        ]

evaluate :: EvaluationInput -> Evaluator a -> String
evaluate ei e = evaluatorPretty e $ evaluatorGather e ei

pointToInput :: EvaluationInputPoint -> EvaluationInput
pointToInput eip = EvaluationInput {eiDiscoveredEqs = eipDiscoveredEqs eip}

lengthEvaluator :: Evaluator Int
lengthEvaluator = Evaluator "length" (length . eiDiscoveredEqs) show
