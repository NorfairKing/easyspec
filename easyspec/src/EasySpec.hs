{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EasySpec where

import Import

import EasySpec.OptParse

import Control.Monad
import Data.IORef

import System.FilePath

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
import Var

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
            -- Doesn't work in a project, only in top-level modules
            modSum <-
                getModSummary $
                mkModuleName $ dropExtension $ toFilePath $ filename setDiscFile
            parsedModule <- parseModule modSum
            tmod <- typecheckModule parsedModule
            let (tcenv, moddets) = tm_internals_ tmod
            let names =
                    concatMap (map gre_name) $ occEnvElts $ tcg_rdr_env tcenv
            forM_ names $ \name -> do
                tything <- lookupName name
                case tything of
                    Just (AnId i) -> printO (i, varType i)
                    Just t -> printO t
                    Nothing -> pure ()

printO
    :: (GhcMonad m, Outputable a)
    => a -> m ()
printO a = do
    dfs <- getProgramDynFlags
    liftIO $ putStrLn $ showPpr dfs a
