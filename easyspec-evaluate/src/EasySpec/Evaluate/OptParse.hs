module EasySpec.Evaluate.OptParse
    ( module EasySpec.Evaluate.OptParse
    , Instructions
    , Dispatch(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import EasySpec.Utils (isSourceFile)

import EasySpec.Evaluate.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags Configuration = (,) <$> disp <*> pure Settings
  where
    disp =
        case cmd of
            CommandEvaluate mdirpath -> do
                let path = fromMaybe "examples" mdirpath
                file <- resolveFile' path
                b <- doesFileExist file
                if b
                    then pure $ DispatchEvaluate [file]
                    else do
                        dir <- resolveDir' $ fromMaybe "examples" mdirpath
                        fs <- snd <$> listDirRecur dir
                        pure $ DispatchEvaluate $ filter isSourceFile fs
            CommandBuild target -> pure $ DispatchBuild target

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Easyspec evaluate"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "evaluate" parseCommandEvaluate
        , command "build" parseCommandBuild
        ]

parseCommandEvaluate :: ParserInfo Command
parseCommandEvaluate = info parser modifier
  where
    parser =
        CommandEvaluate <$>
        argument
            (Just <$> str)
            (mconcat
                 [ metavar "PATH"
                 , value Nothing
                 , help "the path to a file or directory of example/examples"
                 ])
    modifier =
        fullDesc <>
        progDesc "Evaluate different easyspec strategies on examples."

parseCommandBuild :: ParserInfo Command
parseCommandBuild = info parser modifier
  where
    parser =
        CommandBuild <$>
        strArgument (mconcat [metavar "TARGET", help "the target to build."])
    modifier = fullDesc <> progDesc "Build a target in the evaluation system."

parseFlags :: Parser Flags
parseFlags = pure Flags
