module EasySpec.Evaluate where

import Import

import EasySpec.Evaluate.Evaluate
import EasySpec.Evaluate.OptParse

easyspecEvaluate :: IO ()
easyspecEvaluate = do
    (DispatchEvaluate fs, Settings) <- getInstructions
    runEvaluate fs
