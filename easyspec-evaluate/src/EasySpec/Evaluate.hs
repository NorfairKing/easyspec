module EasySpec.Evaluate where

import Import

import EasySpec.Evaluate.Build
import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.OptParse

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    (disp, Settings) <- getInstructions
    case disp of
        DispatchEvaluate fs -> runEvaluate fs
        DispatchBuild t -> runBuild t
        DispatchBuildEverything -> runBuildEverything
