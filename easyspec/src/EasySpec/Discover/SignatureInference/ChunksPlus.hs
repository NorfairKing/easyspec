module EasySpec.Discover.SignatureInference.ChunksPlus
    ( inferChunksPlus
    ) where

import EasySpec.Discover.SignatureInference.ChunksPlusUtils
import EasySpec.Discover.SignatureInference.ShrinkThenDrill
import EasySpec.Discover.Types

inferChunksPlus :: SignatureInferenceStrategy
inferChunksPlus =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus"
    , inferSignature = inferChunksPlusFrom noShrink
    }
