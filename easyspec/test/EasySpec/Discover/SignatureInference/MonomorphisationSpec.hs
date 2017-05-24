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
spec = do
    describe "getKindedTyVars" $ do
        it "works on these simple examples with kind * type variables" $ do
            getKindedTyVars $(easyType "a -> String") `shouldBe`
                ([(Ident () "a", KindStar ())], CxEmpty mempty)
            getKindedTyVars $(easyType "a -> b") `shouldBe`
                ( [(Ident () "a", KindStar ()), (Ident () "b", KindStar ())]
                , CxEmpty mempty)
        it
            "works on this example of a function that is polymorphic in the container" $
            getKindedTyVars $(easyType "m Int -> m String") `shouldBe`
            ( [(Ident () "m", KindFn () (KindStar ()) (KindStar ()))]
            , CxEmpty mempty)
        it
            "works on this example of a function that is polymorphic in the monad" $
            getKindedTyVars $(easyType "Monad m => m Int") `shouldBe`
            ( [(Ident () "m", KindFn () (KindStar ()) (KindStar ()))]
            , CxSingle () $
              ClassA () (UnQual () (Ident () "Monad")) [$(easyType "m")])

    describe "monomorphise" $ do
        it "works on this simple example with a kind * type variable without constraints" $
            monomorphise [$(easyType "Int"), $(easyType "String")] $(easyType "a") `shouldBe` [$(easyType"Int"), $(easyType "String")]
