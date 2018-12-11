{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base    (try)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad.Trans       (liftIO)
import           Data.Either               (either)
import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.IPC
import           HTorrent.Node
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket            (SockAddr (..), Socket (..))
import           System.Environment
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Timeout            (timeout)

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import qualified Data.Text                 as T


hTorrent :: HTMonad ()
hTorrent = do
  -- First begin by converting the torrent information into an initial state
  convertMetaInfo2State
  -- Try and connect to the announcer(s)
  -- This step also does the `connecting` and `announcing` protocol
  -- Also saves the peers obtained from the announcer
  gets _htsAnnouncers >>= connectToAnnounce
  -- After getting list of peers, attempt to do peer wire protocol
  -- with each peer
  peerWireProtocol
  -- TODO: More stuff

initialState = HTState [] [] BS.empty socket BS.empty 0 M.empty M.empty M.empty
  where socket = unsafePerformIO $ NS.socket NS.AF_INET NS.Stream NS.defaultProtocol

main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile torFile
  let decoded = BE.decode contents :: BE.Result MetaInfo
  case decoded of
    Left e  -> putStrLn $ "Torrent File Read Error: " ++ show e
    Right v -> do
      runHTMonad hTorrent (HTEnv v) initialState
      return ()
