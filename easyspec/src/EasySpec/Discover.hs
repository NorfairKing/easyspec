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

import EasySpec.OptParse

import EasySpec.Discover.GatherFromGHC
import EasySpec.Discover.QuickSpec
import EasySpec.Discover.SignatureGeneration
import EasySpec.Discover.TypeTranslation
import EasySpec.Discover.Types

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    ghcIds <- getGHCIds $ setDiscFile ds
    let ids = map toEasyId ghcIds
    se <-
        case createQuickspecSigExp ids of
            Nothing ->
                liftIO $
                die
                    "Unable to generate quickspec signature expression: Not enough type variables in quickspec."
            Just e -> pure $ SignatureExpression e
    runEasySpec ds se
