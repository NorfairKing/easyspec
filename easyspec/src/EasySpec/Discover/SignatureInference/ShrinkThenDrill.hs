module EasySpec.Discover.SignatureInference.ShrinkThenDrill where

import EasySpec.Discover.Types

inferShrinkThenDrill ::
       ([EasyId] -> [EasyId] -> [EasyId])
    -> ([EasyId] -> [EasyId] -> InferredSignature)
    -> [EasyId]
    -> [EasyId]
    -> InferredSignature
inferShrinkThenDrill shrink drill focus scope =
    let shrunkScope = shrink focus scope
    in drill focus shrunkScope

noShrink :: [EasyId] -> [EasyId] -> [EasyId]
noShrink _ scope = scope
