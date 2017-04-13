module EasySpec.Evaluate where

import Import

import EasySpec.Discover as ES
import EasySpec.OptParse as ES

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    exdir <- resolveDir' "examples"
    fs <- (filter ((== ".hs") . fileExtension) . snd) <$> listDirRecur exdir
    forM_ fs $ \f ->
        forM_ inferenceStrategies $ \(name, strat) -> do
            print (name, f)
            let sets = ES.Settings
                ds =
                    ES.DiscoverSettings
                    { setDiscFile = f
                    , setDiscFun = Nothing
                    , setDiscInfStrat = strat
                    }
            runReaderT (ES.discoverEquations ds) sets
