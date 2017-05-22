{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.FullBreakthrough
    ( inferFullBreakthrough
    ) where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferFullBreakthrough :: Int -> SignatureInferenceStrategy
inferFullBreakthrough i =
    breakThroughSplitInferAlg
        ("full-breakthrough-" ++ show i)
        [$(mkRelFile __FILE__)]
        (flip (\\))
        i
