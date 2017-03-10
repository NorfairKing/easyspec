module EasySpec.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandDiscover DiscoverArgs
    deriving (Show, Eq)

data DiscoverArgs = DiscoverArgs
    { argDiscFile :: FilePath
    , argDiscFun :: String
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchDiscover DiscoverSettings
    deriving (Show, Eq)

data DiscoverSettings = DiscoverSettings
    { setDiscFile :: Path Abs File
    , setDiscFun :: String
    } deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
