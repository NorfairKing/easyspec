{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EasySpec.Discover.TH where

import Import

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

import Language.Haskell.Exts as HSE

import EasySpec.Discover.Types

instance Lift (HSE.Exp ())

instance Lift (HSE.Type ())

easyExp :: String -> Q TH.Exp
easyExp s =
    case HSE.parseExp s of
        ParseOk a ->
            let b = () <$ a :: EasyExp
            in [|b|]
        ParseFailed srcLoc err ->
            fail $
            unwords
                [ "parse of expression"
                , show s
                , "failed at:"
                , show srcLoc
                , "with error:"
                , err
                ]

easyType :: String -> Q TH.Exp
easyType s =
    case HSE.parseType s of
        ParseOk a ->
            let b = () <$ a :: EasyType
            in [|b|]
        ParseFailed srcLoc err ->
            fail $
            unwords
                [ "parse of type"
                , show s
                , "failed at:"
                , show srcLoc
                , "with error:"
                , err
                ]
