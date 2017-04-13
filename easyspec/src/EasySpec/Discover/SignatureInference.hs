module EasySpec.Discover.SignatureInference where

import Import

import EasySpec.Discover.Types

inferFullSignature :: SignatureInferenceStrategy
inferFullSignature = inferAlg "full-signature" $ \focus scope -> scope \\ focus

inferEmptySignature :: SignatureInferenceStrategy
inferEmptySignature = inferAlg "empty-signature" $ \_ _ -> []

inferAlg ::
       String
    -> ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> SignatureInferenceStrategy
inferAlg name func =
    SignatureInferenceStrategy name $ \focus scope ->
        let bgSigFuncs = func focus scope
        in InferredSignature
           {sigFocusIds = focus, sigBackgroundIds = bgSigFuncs}
