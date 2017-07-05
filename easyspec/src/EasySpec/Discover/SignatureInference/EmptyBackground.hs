{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.EmptyBackground where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferEmptyBackground :: SignatureInferenceStrategy
inferEmptyBackground =
    splitInferAlg "empty-background" [$(mkRelFile __FILE__)] $ \_ _ -> []
