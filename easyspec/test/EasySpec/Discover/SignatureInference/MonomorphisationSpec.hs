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
    describe "getKindedTyVars" $ do
        it "works on these simple examples with kind * type variables" $ do
            getKindedTyVars $(easyType "a -> String") `shouldBe`
                ([(Ident () "a", KindStar ())], CxEmpty mempty)
            getKindedTyVars $(easyType "a -> b") `shouldBe`
                ( [(Ident () "a", KindStar ()), (Ident () "b", KindStar ())]
                , CxEmpty mempty)
        it
            "works on this example of a function that is polymorphic in the monad" $
            getKindedTyVars $(easyType "Monad m => m Int") `shouldBe`
            ( [(Ident () "m", KindFn () (KindStar ()) (KindStar ()))]
            , CxEmpty mempty)
