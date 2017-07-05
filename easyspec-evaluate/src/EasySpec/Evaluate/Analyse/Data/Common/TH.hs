{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common.TH where

import Import

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES
import qualified EasySpec.OptParse as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Hackage
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types ()
import EasySpec.Utils

-- [(String, [InputSpec])]
buildExamples :: Q Exp
buildExamples = do
    res <- runIO findAllExamples
    TH.lift res

findAllExamples :: MonadIO m => m [(String, [ES.InputSpec])]
findAllExamples =
    sequence [evaluationExamples, runtimeExamples] -- No hackage for now

hackageExamples :: MonadIO m => m (String, [ES.InputSpec])
hackageExamples =
    (,) "hackage" <$> (concat <$> mapM packageExamples hackagePackages)

evaluationExamples :: MonadIO m => m (String, [ES.InputSpec])
evaluationExamples = subdirExamples "evaluation"

runtimeExamples :: MonadIO m => m (String, [ES.InputSpec])
runtimeExamples = subdirExamples "runtime"

subdirExamples :: MonadIO m => String -> m (String, [ES.InputSpec])
subdirExamples dirStr = do
    dir <- liftIO $ parseRelDir dirStr
    edir <- (</> dir) <$> examplesDir
    ss <- sourcesIn edir
    pure (dirStr, map (ES.InputSpec edir) ss)

makeExampleCache :: Q Exp
makeExampleCache = do
    ntups <-
        runIO $ do
            exs <- findAllExamples -- TODO put this function somewhere else so that we can use 'buildExamples' here instead of finding examples again.
            forM (concatMap snd exs) $ \ex -> do
                ns <- findNamesInSource ex
                putStrLn $
                    unlines $
                    [ "Gathering a cache of the functions defined in the following file:"
                    , toFilePath $
                      ES.inputSpecBaseDir ex </> ES.inputSpecFile ex
                    , unwords
                          [ "Found these"
                          , show (length ns)
                          , "functions defined in that file."
                          ]
                    ] ++
                    map ES.prettyPrintOneLine ns
                pure (ex, ns)
    runIO $ do
        td <- tmpDir
        writeJSON (td </> $(mkRelFile "examples.json")) ntups
    TH.lift ntups

findNamesInSource ::
       (MonadIO m, MonadMask m) => ES.InputSpec -> m [ES.EasyQName]
findNamesInSource is =
    (map ES.idName . filter isInSource) <$>
    -- Only take the ones that have an implementation (I.E. are defined in this file.)
    runReaderT (ES.getEasyIds is) ES.Settings {ES.setsDebugLevel = 0}
  where
    isInSource = (== Just (ES.inputSpecFile is)) . ES.idRootloc
