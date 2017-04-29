module EasySpec.Evaluate.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandEvaluate (Maybe FilePath)
    | CommandBuild String
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchEvaluate [Path Abs File]
    | DispatchBuild String
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
