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
    , argDiscBaseDir :: Maybe FilePath
    , argDiscInfStratName :: Maybe String
    , argDiscUnqualified :: Unq
    } deriving (Show, Eq)

data Unq
    = UnqNothing
    | UnqLocal
    | UnqAll
    deriving (Show, Eq)

newtype Flags = Flags
    { flagsDebugLevel :: Maybe Int
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchDiscover DiscoverSettings

data DiscoverSettings = DiscoverSettings
    { setDiscInputSpec :: InputSpec
    , setDiscFun :: Maybe EasyQName
    , setDiscInfStrat :: SignatureInferenceStrategy
    , setDiscQualified :: Unqualification
    }

data Unqualification
    = UnqualifyNothing
    | UnqualifyLocal EasyModuleName
    | UnqualifyAll
    deriving (Show, Eq)

newtype Settings = Settings
    { setsDebugLevel :: Int
    } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings {setsDebugLevel = 0}
