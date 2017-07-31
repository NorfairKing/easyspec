module EasySpec.Discover.SignatureInference.Chunks where

import EasySpec.Discover.SignatureInference.ChunksUtils
import EasySpec.Discover.Types

inferChunks :: SignatureInferenceStrategy
inferChunks = (inferChunksFrom (\_ scope -> scope)) {sigInfStratName = "chunks"}
