{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.Utils where

import Import

import System.FilePath

import DynFlags hiding (Settings)
import GHC
       (GhcMonad, SuccessFlag(..), LoadHowMuch, ModuleName,
        getProgramDynFlags, setSessionDynFlags, load, mkModuleName)
import GHC.LanguageExtensions
import Outputable (Outputable(..), showPpr)

setDFlagsNoLinking :: GhcMonad m => DynFlags -> m ()
setDFlagsNoLinking = void . setSessionDynFlags

loadSuccessfully :: GhcMonad m => LoadHowMuch -> m ()
loadSuccessfully hm = do
    r <- load hm
    case r of
        Succeeded -> pure ()
        Failed -> fail "Loading failed. No idea why."

prepareFlags :: DynFlags -> DynFlags
prepareFlags dflags = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]

getTargetModName :: Path Rel File -> GHC.ModuleName
getTargetModName =
    mkModuleName . dropExtension . filePathToModuleName . toFilePath
  where
    filePathToModuleName = map go
      where
        go '/' = '.'
        go c = c

showGHC :: (GhcMonad m, Outputable a) => a -> m String
showGHC a = do
    dfs <- getProgramDynFlags
    pure $ showPpr dfs a

printO :: (GhcMonad m, Outputable a) => a -> m ()
printO a = showGHC a >>= (liftIO . putStrLn)
