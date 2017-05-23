{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.MonomorphisationSpec
    ( spec
    ) where

import TestImport

import Language.Haskell.Exts.Gen ()
import Language.Haskell.Exts.Syntax

import EasySpec.Discover.SignatureInference.Monomorphisation
import EasySpec.Discover.TH

spec :: Spec
spec =
    describe "getKindedTyVars" $
    it "works on these simple examples with kind * type variables" $ do
        getKindedTyVars $(easyType "a -> String") `shouldBe`
            ([(Ident () "a", KindStar ())], CxEmpty mempty)
        getKindedTyVars $(easyType "a -> b") `shouldBe`
            ( [(Ident () "a", KindStar ()), (Ident () "b", KindStar ())]
            , CxEmpty mempty)
