module Qty.Client (
  hostAndClientMain,
) where

import Qty.Host
import Qty.Types
import Quasar.Async
import Quasar.MonadQuasar
import Quasar.MonadQuasar.Misc
import Quasar.Observable.Core
import Quasar.Observable.ObservableVar
import Quasar.Prelude
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Posix.Pty
import System.Posix.Signals (Handler(..), installHandler)
import System.Posix.Signals.Exts (sigWINCH)
import System.Posix.Types (Fd(..))

hostAndClientMain :: [String] -> IO ()
hostAndClientMain cmd = do
  runQuasarAndExit do
    runHostAndClient cmd >>= \case
      ExitSuccess -> pure ()
      ExitFailure exitCode -> liftIO $
        hPutStrLn stderr $ "qty: Hosted process exited with code " <> show exitCode

runHostAndClient :: (MonadIO m, MonadQuasar m) => [String] -> m ExitCode
runHostAndClient cmd = withClient (\endpoint -> runHost (Just endpoint) cmd)

withClient :: (MonadIO m, MonadQuasar m) => (Client -> m a) -> m a
withClient fn = do
  liftIO (createPty (Fd 1)) >>= \case
    Just terminalPty -> withTerminalClient terminalPty fn
    Nothing -> withPipeClient fn

withTerminalClient ::
  (MonadIO m, MonadQuasar m) =>
  Pty -> (Client -> m a) -> m a
withTerminalClient terminalPty fn = do
  initialAttrs <- liftIO $ setRawMode terminalPty

  winsize <- attachWinsizeObservable terminalPty

  result <- fn $ Client {
    clientName = Nothing,
    clientWinsize = Just winsize
  }

  -- Reset terminal pty to original state
  liftIO $ setTerminalAttributes terminalPty initialAttrs WhenFlushed

  pure result

withPipeClient :: (MonadIO m, MonadQuasar m) => (Client -> m a) -> m a
withPipeClient fn = do
  fn $ Client {
    clientName = Nothing,
    clientWinsize = Nothing
  }

attachWinsizeObservable :: (MonadIO m, MonadQuasar m) => Pty -> m (Observable Load '[] (Int, Int))
attachWinsizeObservable terminalPty = do
  initialWinsize <- liftIO $ ptyDimensions terminalPty
  winsizeVar <- newObservableVarIO initialWinsize

  winchVar <- newTVarIO False
  liftIO $ installWinchHandler (atomically (writeTVar winchVar True))

  async_ do
    liftIO $ forever do
      atomically $ check =<< swapTVar winchVar False
      winsize <- liftIO $ ptyDimensions terminalPty
      atomically $ writeObservableVar winsizeVar winsize

  pure (toObservable winsizeVar)

installWinchHandler :: IO () -> IO ()
installWinchHandler sigWinchAction = void $ installHandler sigWINCH (Catch sigWinchAction) Nothing

-- Set "raw" mode, equivalent to @cfmakeraw@ in @termios(3)@.
--
-- Returns the previously set attributes, to allow restoring the terminal to the
-- original state.
setRawMode :: Pty -> IO TerminalAttributes
setRawMode pty = do
  attrs <- getTerminalAttributes pty
  setTerminalAttributes pty (rawMode attrs) WhenFlushed
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
