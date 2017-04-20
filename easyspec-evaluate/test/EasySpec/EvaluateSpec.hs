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
        forM_ fs $ \f -> do
            funcname <-
                runIO $ do
                    eids <- ES.getEasyIds f
                    generate $ elements $ map ES.idName eids
            forM_ ES.inferenceStrategies $ \is ->
                it (unwords [ES.sigInfStratName is, "runs on", toFilePath f]) $ do
                    ip <- getEvaluationInputPoint f funcname is
                    putStrLn $ showEvaluationReport [[ip]]
