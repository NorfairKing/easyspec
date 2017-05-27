{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Analyse.Data.Common.TH where

import Import

import System.FilePath (dropExtensions)

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

import qualified EasySpec.Discover as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Analyse.Common
import EasySpec.Evaluate.Analyse.Hackage
import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Types ()
import EasySpec.Utils

buildExamples :: Q Exp
buildExamples = do
    res <- runIO findAllExamples
    TH.lift $ map (\(ES.InputSpec bd f) -> (toFilePath bd, toFilePath f)) res

findAllExamples :: MonadIO m => m [ES.InputSpec]
findAllExamples = hackageExamples

hackageExamples :: MonadIO m => m [ES.InputSpec]
hackageExamples = concat <$> mapM packageExamples hackagePackages

contrivedExamples :: MonadIO m => m [ES.InputSpec]
contrivedExamples = do
    edir <- examplesDir
    ss <- sourcesIn edir
    pure $ map (ES.InputSpec edir) ss
