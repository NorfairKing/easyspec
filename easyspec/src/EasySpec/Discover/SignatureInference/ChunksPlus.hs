{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Discover.SignatureInference.ChunksPlus
    ( inferChunksPlus
    ) where

import Import

import EasySpec.Discover.CodeUtils

import EasySpec.Discover.SignatureInference.Chunks
import EasySpec.Discover.SignatureInference.Utils
import EasySpec.Discover.Types

inferChunksPlus :: SignatureInferenceStrategy
inferChunksPlus =
    SignatureInferenceStrategy
    { sigInfStratName = "chunks-plus"
    , sigInfRelevantSources = [$(mkRelFile __FILE__)]
    , inferSignature =
          \focus scope' ->
              let scope = scope' \\ focus
                  terminal = (const $ Just $ makeNamedExps focus, 0, [])
                  tups = zip [1 ..] $ (,) <$> focus <*> scope
                  level1s =
                      flip map tups $ \(i, (ff, otherFunc)) ->
                          (const $ Just $ makeNamedExps [ff, otherFunc], i, [])
                  trips =
                      zip [(length tups + 1) ..] $
                      [(a, b, c) | a <- focus, b <- scope, c <- scope, b < c]
                  level2s =
                      flip map trips $ \(i, (ff, sf1, sf2)) ->
                          let mentionedNodes sf_ =
                                  filter
                                      (\(_, (sf, other)) ->
                                           sf == ff && other == sf_)
                                      tups
                              -- The nodes that mention ff and sf1
                              dependencies =
                                  map fst $
                                  mentionedNodes sf1 ++ mentionedNodes sf2
                              -- If the nodes with the same functions as the functions in this note, so ff, sf1 and sf2,
                              -- all found something relevant, then run this group of three as well.
                              foundSomething (nexs, eqs) =
                                  flip all nexs $ \nex ->
                                      flip any eqs $ \eq ->
                                          mentionsEq (neName nex) eq
                          in ( \prevTups ->
                                   if all foundSomething prevTups
                                       then Just $
                                            nub $ concat $ map fst prevTups
                                       else Nothing
                             , i
                             , dependencies)
              in InferredSignature $ terminal : level1s ++ level2s
    }
  where
    makeNamedExps funcs = rights $ map convertToUsableNamedExp funcs
