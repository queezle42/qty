module Qty.Host (
  hostMain,
) where

import Control.Exception (handle)
import Data.ByteString qualified as BS
import Quasar.Async
import Quasar.MonadQuasar.Misc
import Quasar.Prelude
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stdin, stdout, stderr)
import System.Posix.Pty
import System.Posix.Signals (Handler(..), installHandler)
import System.Posix.Signals.Exts (sigWINCH)
import System.Posix.Types (Fd(..))
import System.Process

hostMain :: [String] -> IO ()
hostMain [] = putStrLn "TODO find user shell"
hostMain (program:args) = runHost program args

runHost :: String -> [String] -> IO ()
runHost program args = runQuasarAndExitWith toExitCode do
  exitCode <- withRawMode do

    (pty, processHandle) <- liftIO $ spawnWithPty
      Nothing
      True -- search in PATH
      program
      args
      (120, 40) -- initial dimensions

    -- pty to stdout
    async_ $ liftIO $ handle (\(_ :: IOException) -> pure ()) do
      forever do
        chunk <- readPty pty
        BS.hPutStr stdout chunk

    -- stdin to pty
    async_ $ liftIO $ forever do
      chunk <- BS.hGetSome stdin 4096
      writePty pty chunk

    liftIO $ waitForProcess processHandle

  liftIO $ hPutStrLn stderr $ "qty: Process exited with code " <> show exitCode
  pure exitCode

toExitCode :: QuasarExitState ExitCode -> ExitCode
toExitCode (QuasarExitSuccess exitCode) = exitCode
toExitCode (QuasarExitAsyncException exitCode) = exitCode
toExitCode QuasarExitMainThreadFailed = ExitFailure 255

withRawMode :: MonadIO m => m a -> m a
withRawMode action = do
  liftIO (createPty (Fd 1)) >>= \case
    Nothing -> action
    Just terminalPty -> do
      attrs <- liftIO $ setRawMode terminalPty
      result <- action
      liftIO $ setTerminalAttributes terminalPty attrs WhenFlushed
      pure result

-- Set "raw" mode, equivalent to @cfmakeraw@ in @termios(3)@.
--
-- Returns the previously set attributes, to allow restoring the terminal to the
-- original state.
setRawMode :: Pty -> IO TerminalAttributes
setRawMode pty = do
  attrs <- getTerminalAttributes pty
  setTerminalAttributes pty (rawMode attrs) Immediately
  pure attrs
  where
    rawMode :: TerminalAttributes -> TerminalAttributes
    rawMode =
      -- Input flags
      (`withoutMode` IgnoreBreak) .
      (`withoutMode` InterruptOnBreak) .
      (`withoutMode` MarkParityErrors) .
      (`withoutMode` StripHighBit) .
      (`withoutMode` MapLFtoCR) .
      (`withoutMode` IgnoreCR) .
      (`withoutMode` MapCRtoLF) .
      (`withoutMode` StartStopOutput) .
      -- Output flags
      (`withoutMode` ProcessOutput) .
      (`withoutMode` EnableParity) .
      -- Local modes
      (`withoutMode` EnableEcho) .
      (`withoutMode` EchoLF) .
      (`withoutMode` ProcessInput) .
      (`withoutMode` KeyboardInterrupts) .
      (`withoutMode` ExtendedFunctions) .
      -- Character size
      (`withBits` 8)
