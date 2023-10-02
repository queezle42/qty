{-# LANGUAGE ApplicativeDo #-}

module Qty.Cli (
  main,
) where

import Quasar.Prelude
import Options.Applicative
import Qty.Host qualified

-- | Main entry point and argument parser.
main :: IO ()
main = join (customExecParser (prefs showHelpOnEmpty) parser)
  where
    parser :: ParserInfo (IO ())
    parser = info (mainParser <**> helper)
      (fullDesc <> header "network-counting - quasar example that counts connected clients")

    mainParser :: Parser (IO ())
    mainParser = hsubparser (
        command "run" (info (Qty.Host.hostMain <$> hostParser) (progDesc "Host a process in a pty and connect to it in the current terminal."))
      )

    hostParser :: Parser [String]
    hostParser = do
      many $ strArgument $ metavar "CMD" <> help "Command and arguments. Leave empty to launch a shell."

    connectParser :: Parser (IO ())
    connectParser = undefined
