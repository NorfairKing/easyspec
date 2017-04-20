{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Utils where

import Import

import EasySpec.OptParse.Types

debugLvl
    :: MonadReader Settings m
    => m Int
debugLvl = asks setsDebugLevel

whenDebugGE
    :: MonadReader Settings m
    => Int -> m a -> m ()
whenDebugGE i func = do
    l <- debugLvl
    when (l >= i) $ void func

whenDebug1
    :: MonadReader Settings m
    => m a -> m ()
whenDebug1 = whenDebugGE 1

debug1
    :: (MonadIO m, MonadReader Settings m)
    => String -> m ()
debug1 s =
    whenDebug1 $
    liftIO $ forM (lines s) $ \l -> putStrLn $ unwords ["[DEBUG]", l]
