module EasySpec.Discover.SignatureInference.IterativeChunks where

import Import

import EasySpec.Discover.SignatureInference.SimilarityUtils
import EasySpec.Discover.SignatureInference.SyntacticSimilarityType
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferIterativeChunks :: Int -> Int -> SignatureInferenceStrategy
inferIterativeChunks i stepSize =
    SignatureInferenceStrategy
    { sigInfStratName =
          intercalate "-" ["iterative-chunks", show i, show stepSize]
    , inferSignature =
          \focus scope ->
              InferredSignature $
              inferIterative
                  i
                  stepSize
                  (`diffChoice` simDiffType)
                  ranking
                  focus
                  scope
    }
  where
    ranking level1t focus scope = do
        let tups = (,) <$> focus <*> scope
        rs <-
            forM tups $ \(ff, otherFunc) ->
                (,) otherFunc <$>
                inferFromWith (makeNamedExps [ff, otherFunc]) [level1t]
        pure $ map fst $ reverse $ sortOn (length . snd) rs
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs

inferIterative ::
       Int -- ^ Size of shrinker
    -> Int -- ^ Step size
    -> (Int -> [EasyId] -> [EasyId] -> [EasyId]) -- ^ A way to shrink that can specify the maximum size of the result
    -> (OptiToken -> [EasyId] -> [EasyId] -> InferM [EasyId]) -- ^ A way to rank scope functions according to 'goodness' with the best first
    -> [EasyId]
    -> [EasyId]
    -> InferM ()
inferIterative i stepSize shrink rank focus scope = do
    level1t <- inferFrom_ $ makeNamedExps focus
        -- Get a 'level 1' discovery to get that out of the way.
    let go j currentfocus =
            if j <= 0
                then pure []
                else do
                    let scope' =
                            nub $
                            flip concatMap currentfocus $ \ff ->
                                shrink j [ff] scope
                        -- Shrink the current scope to a constant-sized sub-scope around the focus
                    newFocus <-
                        take (j - stepSize) <$> rank level1t focus scope'
                        -- Rank that subscope, and take the (i-1) best ones functions.
                    go (j - stepSize) newFocus
    void $ go i focus
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
