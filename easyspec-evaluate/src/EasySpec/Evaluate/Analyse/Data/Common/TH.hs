{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common.TH where

import Import

import Path.Internal (Path(Path))
import System.FilePath (dropExtensions)

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

-- [(FilePath, FilePath)]
buildExamples :: Q Exp
buildExamples = do
    res <- runIO findAllExamples
    runIO $ print res
    TH.lift res

findAllExamples :: MonadIO m => m [ES.InputSpec]
findAllExamples = liftM2 (++) hackageExamples contrivedExamples

hackageExamples :: MonadIO m => m [ES.InputSpec]
hackageExamples = concat <$> mapM packageExamples hackagePackages

contrivedExamples :: MonadIO m => m [ES.InputSpec]
contrivedExamples = do
    edir <- examplesDir
    ss <- sourcesIn edir
    pure $ map (ES.InputSpec edir) ss

makeExampleCache :: Q Exp
makeExampleCache = do
    ntups <-
        runIO $ do
            exs <- findAllExamples
            forM exs $ \ex -> do
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
                    map (ES.prettyPrintOneLine) ns
                pure (ex, ns)
    TH.lift ntups

findNamesInSource :: (MonadIO m, MonadMask m) => ES.InputSpec -> m [ES.EasyName]
findNamesInSource is =
    (map ES.idName . filter isInSource) <$>
    -- Only take the ones that have an implementation (I.E. are defined in this file.)
    runReaderT (ES.getEasyIds is) ES.Settings {ES.setsDebugLevel = 0}
  where
    isInSource = isJust . ES.idImpl
