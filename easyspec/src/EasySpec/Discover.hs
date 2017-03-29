{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover where

import Import

import EasySpec.OptParse

import EasySpec.Discover.GHC
import EasySpec.Discover.Gather

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    ids <- getIds ds
    runEasySpec ds ids
