{-# LANGUAGE FlexibleContexts #-}

module EasySpec where

import Import

import EasySpec.Discover
import EasySpec.OptParse

easyspec :: IO ()
easyspec = do
    (disp, sets) <- getInstructions
    runReaderT (dispatch disp) sets

dispatch
    :: (MonadIO m, MonadReader Settings m)
    => Dispatch -> m ()
dispatch (DispatchDiscover ds) = discover ds
