{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec where

import Import

import EasySpec.OptParse

import Data.IORef

import DynFlags hiding (Settings)
import GHC
import GHC.LanguageExtensions
import GHC.Paths (libdir)
import HscTypes
import NameEnv
import OccName
import Outputable
import RdrName
import TcRnTypes

easyspec :: IO ()
easyspec = do
    (disp, sets) <- getInstructions
    runReaderT (dispatch disp) sets

dispatch
    :: (MonadIO m, MonadReader Settings m)
    => Dispatch -> m ()
dispatch (DispatchDiscover DiscoverSettings {..}) = do
    sets <- ask
    liftIO $
        runGhc (Just libdir) $ do
            liftIO $ print sets
            dflags <- getSessionDynFlags
            let compdflags =
                    (foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash])
            setSessionDynFlags compdflags
            target <- guessTarget (toFilePath setDiscFile) Nothing
            setTargets [target]
            load LoadAllTargets
            modSum <- getModSummary $ mkModuleName "Reverse"
            parsedModule <- parseModule modSum
            tmod <- typecheckModule parsedModule
            -- printO $ tm_renamed_source tmod
            -- printO $ tm_typechecked_source tmod
            let (tcenv, moddets) = tm_internals_ tmod
            -- printO $ tcg_rn_exports tcenv
            printO $ map (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
            -- printO $ tcg_binds tcenv
            -- printO $ tcg_rn_exports tcenv
            -- env <- liftIO $ readIORef $ tcg_type_env_var tcenv
            -- printO $ typeEnvElts env
            -- printO $ typeEnvElts $ tcg_type_env tcenv
            -- printO $ tcg_binds tcenv
            -- printO $ tcg_tcs tcenv

printO
    :: (GhcMonad m, Outputable a)
    => a -> m ()
printO a = do
    dfs <- getProgramDynFlags
    liftIO $ putStrLn $ showPpr dfs a
