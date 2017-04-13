module EasySpec.OptParse.Types where

import Import

import EasySpec.Discover.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandDiscover DiscoverArgs
    deriving (Show, Eq)

data DiscoverArgs = DiscoverArgs
    { argDiscFile :: FilePath
    , argDiscFun :: Maybe String
    , argDiscInfStratName :: Maybe String
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchDiscover DiscoverSettings

data DiscoverSettings = DiscoverSettings
    { setDiscFile :: Path Abs File
    , setDiscFun :: Maybe EasyName
    , setDiscInfStrat :: SignatureInferenceStrategy
    }

data Settings =
    Settings
    deriving (Show, Eq)
