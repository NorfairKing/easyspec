module EasySpec.Discover.SignatureInference.Chunks where

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.SignatureInference.ShrinkThenDrill
import EasySpec.Discover.Types

inferChunks :: SignatureInferenceStrategy
inferChunks =
    SignatureInferenceStrategy
    {sigInfStratName = "chunks", inferSignature = inferChunksFrom noShrink}
