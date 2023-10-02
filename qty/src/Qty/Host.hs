module Qty.Host (
  hostMain,
  runHost,
) where

import Control.Exception (handle)
import Data.ByteString qualified as BS
import Qty.Types
import Quasar.Async
import Quasar.MonadQuasar
import Quasar.MonadQuasar.Misc
import Quasar.Prelude
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stdin, stdout, stderr)
import System.Posix.Pty
import System.Process
import Quasar.Observable.Core (retrieveObservable)

hostMain :: [String] -> IO ()
hostMain cmd = do
  runQuasarAndExit do
    runHost Nothing cmd >>= \case
      ExitSuccess -> pure ()
      ExitFailure exitCode -> liftIO $
        hPutStrLn stderr $ "qty: Hosted process exited with code " <> show exitCode

cmdToProgramAndArgs :: [String] -> IO (String, [String])
cmdToProgramAndArgs [] = fail "TODO find user shell using getent"
cmdToProgramAndArgs (program:args) = pure (program, args)

runHost :: (MonadIO m, MonadQuasar m) => Maybe Client -> [String] -> m ExitCode
runHost initialClient cmd = do
  (program, args) <- liftIO $ cmdToProgramAndArgs cmd

  winsize <- case initialClient of
    Just ((.clientWinsize) -> Just winsizeObservable) ->
      retrieveObservable winsizeObservable
    _ -> pure (120, 40) -- initial dimensions

  env <- liftIO getEnvironment
  (pty, processHandle) <- liftIO $ spawnWithPty
    (Just (("QTY", "1") : env))
    True -- search in PATH
    program
    args
    winsize

  -- pty to stdout
  async_ $ liftIO $ handle (\(_ :: IOException) -> pure ()) do
    forever do
      chunk <- readPty pty
      -- TODO send to clients instead
      BS.hPutStr stdout chunk

  -- stdin to pty
  async_ $ liftIO $ forever do
      -- TODO get from clients instead
    chunk <- BS.hGetSome stdin 4096
    writePty pty chunk

  liftIO $ waitForProcess processHandle

toExitCode :: QuasarExitState ExitCode -> ExitCode
toExitCode (QuasarExitSuccess exitCode) = exitCode
toExitCode (QuasarExitAsyncException exitCode) = exitCode
toExitCode QuasarExitMainThreadFailed = ExitFailure 255
