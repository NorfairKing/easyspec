module EasySpec.Evaluate.EvaluateSpec
    ( spec
    ) where

import TestImport

import EasySpec.Evaluate.Evaluate

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse.Types as ES

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
                    eids <- runReaderT (ES.getEasyIds f) ES.defaultSettings
                    generate $ elements $ map ES.idName eids
            forM_ ES.inferenceStrategies $ \is ->
                it (unwords [ES.sigInfStratName is, "runs on", toFilePath f]) $ do
                    ip <- getEvaluationInputPoint f funcname is
                    putStrLn $ showEvaluationReport [[ip]]
