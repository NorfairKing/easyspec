{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module EasySpec.Evaluate.Analyse.Data.Types where

import Import

import Development.Shake.Classes

newtype Equations =
    Equations ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
