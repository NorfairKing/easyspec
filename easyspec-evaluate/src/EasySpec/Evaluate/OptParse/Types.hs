module EasySpec.Evaluate.OptParse.Types where

import Import

import qualified EasySpec.Discover.Types as ES

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandEvaluate (Maybe FilePath)
                      (Maybe FilePath)
    | CommandBuild String
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchEvaluate [ES.InputSpec]
    | DispatchBuild String
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
