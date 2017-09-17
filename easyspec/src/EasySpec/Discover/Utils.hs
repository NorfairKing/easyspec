{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module EasySpec.Discover.Utils where

import Import
import GHC
import GHC.Paths (libdir)

import qualified Data.Set as Set
import DynFlags hiding (Settings)
import GHC
       (GhcMonad, LoadHowMuch, SuccessFlag(..),
        getProgramDynFlags, load, setSessionDynFlags)
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

filePathToModuleName :: MonadIO m => Path Abs File -> m String
filePathToModuleName (toFilePath -> p) = liftIO . runGhc (Just libdir) $ do
  target <- guessTarget p Nothing
  setTargets [target]
  g <- depanal [] True
  case [ m | m <- g, Just p == ml_hs_file (ms_location m) ] of
    [] -> fail $ "Couldn't get ModSummary for " ++ p
    m : _ -> pure . moduleNameString $ ms_mod_name m

showGHC :: (GhcMonad m, Outputable a) => a -> m String
showGHC a = do
    dfs <- getProgramDynFlags
    pure $ showPpr dfs a

printO :: (GhcMonad m, Outputable a) => a -> m ()
printO a = showGHC a >>= (liftIO . putStrLn)

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList
