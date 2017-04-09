module EasySpec.Discover.SignatureInference where

import Import

import EasySpec.Discover.Types

inferFullSignature ::
       [EasyId] -- ^ Focus functions
    -> [EasyId] -- ^ Full scope (can still contain the interesting functions as well)
    -> InferredSignature
inferFullSignature = inferAlg $ \focus scope -> scope \\ focus

inferEmptySignature ::
       [EasyId] -- ^ Focus functions
    -> [EasyId] -- ^ Full scope (can still contain the interesting functions as well)
    -> InferredSignature
inferEmptySignature = inferAlg $ \_ _ -> []

inferAlg ::
       ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> [EasyId]
    -> [EasyId]
    -> InferredSignature
inferAlg func focus scope =
    let bgSigFuncs = func focus scope
    in InferredSignature {sigFocusIds = focus, sigBackgroundIds = bgSigFuncs}
