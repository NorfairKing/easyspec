module EasySpec.Discover.SourceGathering where

import Import

import GHC

import Language.Haskell.Exts.Syntax as HSE

import EasySpec.Discover.Types
import EasySpec.Discover.GatherFromGHC

gatherSourceOf :: MonadIO m => IdData -> m (Maybe EasyImpl)
gatherSourceOf = undefined
