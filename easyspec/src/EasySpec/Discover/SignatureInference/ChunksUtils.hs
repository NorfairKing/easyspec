module EasySpec.Discover.SignatureInference.ChunksUtils where

import Import

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksFrom ::
       ([EasyId] -> [EasyId] -> [EasyId]) -> SignatureInferenceStrategy
inferChunksFrom scopeMod =
    SignatureInferenceStrategy
    { sigInfStratName = undefined
    , inferSignature =
          \focus scope' ->
              InferredSignature $ do
                  let scope = scopeMod focus scope'
                  level1t <- inferFrom_ $ makeNamedExps focus
                  let tups = (,) <$> focus <*> scope
                  forM_ tups $ \(ff, otherFunc) ->
                      inferFromWith (makeNamedExps [ff, otherFunc]) [level1t]
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
