module EasySpec.Evaluate where

import Import

import EasySpec.Evaluate.OptParse

import qualified EasySpec.Discover as ES
import qualified EasySpec.OptParse as ES

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    (DispatchEvaluate exdir, Settings) <- getInstructions
    fs <- (filter ((== ".hs") . fileExtension) . snd) <$> listDirRecur exdir
    forM_ fs $ \f ->
        forM_ ES.inferenceStrategies $ \(name, strat) -> do
            print (name, f)
            let sets = ES.Settings
                ds =
                    ES.DiscoverSettings
                    { ES.setDiscFile = f
                    , ES.setDiscFun = Nothing
                    , ES.setDiscInfStrat = strat
                    }
            runReaderT (ES.discoverEquations ds) sets
