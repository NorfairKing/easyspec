{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.TypeReachability where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferTypeReachability :: SignatureInferenceStrategy
inferTypeReachability =
    splitInferAlg "type-reachability" [$(mkRelFile __FILE__)] $ \_ _ -> []
