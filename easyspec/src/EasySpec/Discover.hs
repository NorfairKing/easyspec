{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Property discovery happens in multiple steps:

- All the relevant 'GHC.Id's are gathered from a given source file.
- The 'GHC.Id's are converted to 'EasyId's
- The EasyId's are converted to an 'EasyExp' that represents the signature as input to quickspec
- Quickspec is run interactively

-}
module EasySpec.Discover
    ( discover
    , discoverEquations
    , inferenceStrategies
    , inferEmptySignature
    , inferFullSignature
    ) where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import EasySpec.OptParse.Types

import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.QuickSpec
import EasySpec.Discover.SignatureInference
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    res <- discoverEquations ds
    liftIO $
        mapM_
            (\(EasyEq lh rh) ->
                 putStrLn $ prettyPrint lh ++ " = " ++ prettyPrint rh)
            res

discoverEquations ::
       (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m [EasyEq]
discoverEquations ds = do
    ghcIds <- getGHCIds $ setDiscFile ds
    let ids = map toEasyId ghcIds
    let SignatureInferenceStrategy inferStrat = setDiscInfStrat ds
    let iSig = uncurry inferStrat $ splitFocus ds ids
    runEasySpec ds iSig

splitFocus :: DiscoverSettings -> [EasyId] -> ([EasyId], [EasyId])
splitFocus ds ids =
    let fs =
            case find (\i -> Just (prettyPrint $ idName i) == setDiscFun ds) ids of
                Nothing -> []
                Just i -> [i]
    in (fs, ids \\ fs)

inferenceStrategies :: [(String, SignatureInferenceStrategy)]
inferenceStrategies =
    [ ("empty-signature", inferEmptySignature)
    , ("full-signature", inferFullSignature)
    ]
