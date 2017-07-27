module EasySpec.Discover.SignatureInference.ChunksPlus
    ( inferChunksPlus
    ) where

import Import

import EasySpec.Discover.CodeUtils

import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksPlus :: SignatureInferenceStrategy
inferChunksPlus =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus"
    , inferSignature =
          \focus scope' ->
              InferredSignature $ do
                  let scope = scope' \\ focus
                  level1t <- inferFrom_ $ makeNamedExps focus
                  let tups = (,) <$> focus <*> scope
                  level2Tups <-
                      forM tups $ \(ff, otherFunc) ->
                          let nexs = makeNamedExps [ff, otherFunc]
                          in (,,) (ff, otherFunc) nexs <$>
                             inferFromWith nexs [level1t]
                  let trips =
                          [ (a, b, c)
                          | a <- focus
                          , b <- scope
                          , c <- scope
                          , b < c
                          ]
                  forM_ trips $ \(ff, sf1, sf2) -> do
                      let mentionedNodes sf_ =
                              filter
                                  (\((sf, other), _, _) ->
                                       sf == ff && other == sf_)
                                  level2Tups
                          -- The nodes that mention ff and sf1
                          dependencies =
                              mentionedNodes sf1 ++ mentionedNodes sf2
                          -- If the nodes with the same functions as the functions in this note, so ff, sf1 and sf2,
                          -- all found something relevant, then run this group of three as well.
                          foundSomething (_, nexs, (_, eqs)) =
                              flip all nexs $ \nex ->
                                  flip any eqs $ \eq ->
                                      mentionsEq (neName nex) eq
                      when (all foundSomething dependencies) $ do
                          let ns =
                                  nub $
                                  concatMap (\(_, nexs, _) -> nexs) dependencies
                              tokens = map (\(_, _, (t, _)) -> t) dependencies
                          void $ inferFromWith ns tokens
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
