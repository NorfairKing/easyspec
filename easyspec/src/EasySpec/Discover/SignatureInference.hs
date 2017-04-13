module EasySpec.Discover.SignatureInference where

import Import

import EasySpec.Discover.Types

inferFullSignature :: SignatureInferenceStrategy
inferFullSignature = inferAlg $ \focus scope -> scope \\ focus

inferEmptySignature :: SignatureInferenceStrategy
inferEmptySignature = inferAlg $ \_ _ -> []

inferAlg ::
       ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> SignatureInferenceStrategy
inferAlg func =
    SignatureInferenceStrategy $ \focus scope ->
        let bgSigFuncs = func focus scope
        in InferredSignature
           {sigFocusIds = focus, sigBackgroundIds = bgSigFuncs}
