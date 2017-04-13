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

import EasySpec.Discover
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
                    let infStrat =
                            fromMaybe inferFullSignature $
                            argDiscInfStratName >>=
                            (`lookup` inferenceStrategies)
                    pure
                        DiscoverSettings
                        { setDiscFile = file
                        , setDiscFun = argDiscFun
                        , setDiscInfStrat = infStrat
                        }

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
         argument
             (Just <$> str)
             (mconcat
                  [ metavar "FUNCTION"
                  , value Nothing
                  , help "The function to discover properties of"
                  ]) <*>
         option
             (Just <$> str)
             (mconcat
                  [ metavar "SIGINFALG"
                  , value Nothing
                  , help "The name of the signature inference algorithm to use"
                  ]))
    modifier = fullDesc <> progDesc "Command example."

parseFlags :: Parser Flags
parseFlags = pure Flags
