module EasySpec.Discover.SignatureInference.IterativeChunks where

import Import

import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferIterativeChunks :: Int -> SignatureInferenceStrategy
inferIterativeChunks i =
    SignatureInferenceStrategy
    { sigInfStratName = "iterative-chunks"
    , inferSignature =
          \focus scope ->
              InferredSignature $ do
                  let scope' = diffChoice i simDiffType focus scope
                  level1t <- inferFrom_ $ makeNamedExps focus
                  let tups = (,) <$> focus <*> scope
                  ranking <-
                      forM tups $ \(ff, otherFunc) ->
                          (,) otherFunc <$>
                          inferFromWith
                              (makeNamedExps [ff, otherFunc])
                              [level1t]
                  let newFocus =
                          map fst $
                          take (i - 1) $ reverse $ sortOn (length . snd) ranking
                  let newTups =
                          flip concatMap newFocus $ \nff ->
                              let nfClose =
                                      diffChoice
                                          (i - 1)
                                          simDiffType
                                          [nff]
                                          (scope \\ scope')
                              in (,) <$> focus <*> nfClose
                  forM_ (nub newTups) $ \(ff', nfcf') ->
                      inferFromWith (makeNamedExps [ff', nfcf']) [level1t]
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
