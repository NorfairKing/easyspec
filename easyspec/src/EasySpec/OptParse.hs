{-# LANGUAGE RecordWildCards #-}

module EasySpec.OptParse
    ( getInstructions
    , Dispatch(..)
    , DiscoverSettings(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import EasySpec.OptParse.Types

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
            CommandDiscover DiscoverArgs {..} ->
                DispatchDiscover <$> do
                    file <- resolveFile' argDiscFile
                    pure
                        DiscoverSettings
                        {setDiscFile = file, setDiscFun = argDiscFun}

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
    description = "Easyspec"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "discover" parseCommandDiscover]

parseCommandDiscover :: ParserInfo Command
parseCommandDiscover = info parser modifier
  where
    parser =
        CommandDiscover <$>
        (DiscoverArgs <$>
         strArgument (mconcat [metavar "FILE", help "The file to look in"]) <*>
         option
             (Just <$> str)
             (mconcat
                  [ metavar "FUNCTION"
                  , value Nothing
                  , help "The function to discover properties of"
                  ]))
    modifier = fullDesc <> progDesc "Command example."

parseFlags :: Parser Flags
parseFlags = pure Flags
