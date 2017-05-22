{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.FullBreakthrough
    ( inferFullBreakthrough
    ) where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferFullBreakthrough :: SignatureInferenceStrategy
inferFullBreakthrough =
    breakThroughSplitInferAlg
        "full-breakthrough"
        [$(mkRelFile __FILE__)]
        (flip (\\))
        1
