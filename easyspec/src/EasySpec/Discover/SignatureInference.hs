module EasySpec.Discover.SignatureInference where

import Import

import Data.Tree

import EasySpec.Discover.Types

inferFullSignature :: SignatureInferenceStrategy
inferFullSignature =
    splitInferAlg "full-signature" $ \focus scope -> scope \\ focus

inferEmptySignature :: SignatureInferenceStrategy
inferEmptySignature = splitInferAlg "empty-signature" $ \_ _ -> []

splitInferAlg ::
       String
    -> ([EasyId] -> [EasyId] -> [EasyId]) -- ^ Something that chooses the background ids.
    -> SignatureInferenceStrategy
splitInferAlg name func =
    SignatureInferenceStrategy name $ \focus scope ->
        let bgSigFuncs = func focus scope
        in InferredSignature $ Node focus [Node bgSigFuncs []]
