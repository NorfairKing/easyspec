module EasySpec.Discover.SignatureInference.ChunksUtils where

import Import

import EasySpec.Discover.SignatureInference.ShrinkThenDrill
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksFrom ::
       ([EasyId] -> [EasyId] -> [EasyId])
    -> [EasyId]
    -> [EasyId]
    -> InferredSignature
inferChunksFrom shrink = inferShrinkThenDrill shrink drillChunks

drillChunks :: [EasyId] -> [EasyId] -> InferredSignature
drillChunks focus scope =
    InferredSignature $ do
        level1t <- inferFrom_ $ makeNamedExps focus
        let tups = (,) <$> focus <*> scope
        forM_ tups $ \(ff, otherFunc) ->
            inferFromWith (makeNamedExps [ff, otherFunc]) [level1t]
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
