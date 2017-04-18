module EasySpec.EvaluateSpec
    ( spec
    ) where

import TestImport

import EasySpec.Evaluate

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

spec :: Spec
spec =
    describe "easyspec-evaluate" $ do
        fs <-
            runIO $ do
                dir <- resolveDir' "../examples"
                (filter ((== ".hs") . fileExtension) . snd) <$> listDirRecur dir
        forM_ fs $ \f ->
            it
                (unwords
                     [ "runs on"
                     , toFilePath f
                     , "with all signature inference strategies"
                     ]) $ do
                eids <- ES.getEasyIds f
                funcname <- generate $ elements $ map ES.idName eids
                eips <- getEvaluationInputPointsForName f funcname
                putStrLn $ showEvaluationReport [eips]
