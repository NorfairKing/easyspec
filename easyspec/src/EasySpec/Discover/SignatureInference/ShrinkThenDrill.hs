module EasySpec.Discover.SignatureInference.ShrinkThenDrill where

import Import

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

composeShrinks ::
       ([EasyId] -> [EasyId] -> [EasyId])
    -> ([EasyId] -> [EasyId] -> [EasyId])
    -> [EasyId]
    -> [EasyId]
    -> [EasyId]
composeShrinks s1 s2 focus = s2 focus . s1 focus
