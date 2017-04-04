module EasySpec.Discover.SignatureInference where

import Import

import EasySpec.Discover.Types

inferFullSignature ::
       [EasyId] -- ^ Focus functions
    -> [EasyId] -- ^ Full scope (can still contain the interesting functions as well)
    -> InferredSignature
inferFullSignature focus scope =
    InferredSignature {sigFocusIds = focus, sigBackgroundIds = scope \\ focus}
