{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.TypeReachabilitySpec
    ( spec
    ) where

import TestImport

import Language.Haskell.Exts.Gen ()

import EasySpec.Discover.SignatureInference.TypeReachability
import EasySpec.Discover.TH

spec :: Spec
spec =
    describe "typeReachable" $ do
        it "is symmetric" $ symmetry typeReachable
        it "works on this simple example" $
            typeReachable
                $(easyType "Int -> String")
                $(easyType "String -> Double")
        it "works on this example With a unit" $ do
            typeReachable $(easyType "Int -> ()") $(easyType "() -> Double") `shouldBe`
                True
            typeReachable $(easyType "Double -> ()") $(easyType "() -> Int") `shouldBe`
                True
