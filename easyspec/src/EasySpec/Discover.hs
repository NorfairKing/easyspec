{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EasySpec.Discover where

import Import

import Language.Haskell.Exts.Pretty

import EasySpec.OptParse

import EasySpec.Discover.GHC
import EasySpec.Discover.Gather
import EasySpec.Discover.Types

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    ids <- getIds $ setDiscFile ds
    liftIO $ mapM_ (putStrLn . prettyPrint . idType) ids
    runEasySpec ds ids
