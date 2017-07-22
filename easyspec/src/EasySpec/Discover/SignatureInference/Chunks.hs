{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.Chunks where

import Import

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunks :: SignatureInferenceStrategy
inferChunks =
    (inferChunksFrom (\_ scope -> scope))
    { sigInfStratName = "chunks"
    , sigInfRelevantSources = [$(mkRelFile __FILE__)]
    }
