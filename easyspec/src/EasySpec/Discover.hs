{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EasySpec.Discover where

import Import

import Language.Haskell.Exts.Pretty

import EasySpec.OptParse

import EasySpec.Discover.GHC
import EasySpec.Discover.Gather

discover :: (MonadIO m, MonadReader Settings m) => DiscoverSettings -> m ()
discover ds = do
    ids <- getIds ds
    liftIO $ mapM_ (putStrLn . prettyPrint . easyType . toEasyId) ids
    runEasySpec ds ids
