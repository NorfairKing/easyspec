{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Property discovery happens in multiple steps:

- All the relevant 'GHC.Id's are gathered from a given source file.
- The 'GHC.Id's are converted to 'EasyId's
- The EasyId's are converted to an 'EasyExp' that represents the signature as input to quickspec
- Quickspec is run interactively

-}
module EasySpec.Discover where

import Import

import Language.Haskell.Exts.Pretty (prettyPrint)

import EasySpec.OptParse

import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.QuickSpec
import EasySpec.Discover.SignatureInference
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    ghcIds <- getGHCIds $ setDiscFile ds
    let ids = map toEasyId ghcIds
    let iSig = uncurry inferFullSignature $ splitFocus ds ids
    res <- runEasySpec ds iSig
    liftIO $
        mapM_
            (\(lh :=: rh) ->
                 putStrLn $ prettyPrint lh ++ " = " ++ prettyPrint rh)
            res

splitFocus :: DiscoverSettings -> [EasyId] -> ([EasyId], [EasyId])
splitFocus ds ids =
    let fs =
            case find (\i -> Just (prettyPrint $ idName i) == setDiscFun ds) ids of
                Nothing -> []
                Just i -> [i]
    in (fs, ids \\ fs)
