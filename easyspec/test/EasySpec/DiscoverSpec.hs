{-# LANGUAGE TemplateHaskell #-}

module EasySpec.DiscoverSpec
    ( spec
    ) where

import TestImport

import Language.Haskell.Exts.Syntax

import EasySpec.Discover
import EasySpec.Discover.SignatureInference
import EasySpec.Discover.Types
import EasySpec.OptParse.Types

spec :: Spec
spec =
    describe "discover" $ do
        exampleDir <- runIO $ resolveDir' "../examples/toy"
        it "works on 'Reachability.hs' with focus 'g' and finds some equations" $ do
            rels <-
                flip runReaderT defaultSettings $
                discoverRelevantEquations
                    DiscoverSettings
                    { setDiscInputSpec =
                          InputSpec
                          { inputSpecBaseDir = exampleDir
                          , inputSpecFile = $(mkRelFile "Reachability.hs")
                          }
                    , setDiscFun =
                          Just $
                          Qual
                              mempty
                              (ModuleName mempty "Reachability")
                              (Ident mempty "g")
                    , setDiscInfStrat = inferFullBackground
                    , setDiscQualified = UnqualifyNothing
                    }
            rels `shouldSatisfy` (not . null)
