{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.FullBackground where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferFullBackground :: SignatureInferenceStrategy
inferFullBackground =
    splitInferAlg "full-background" [$(mkRelFile __FILE__)] (flip (\\))
