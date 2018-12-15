{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad.Trans       (liftIO)
import           HTorrent.Node
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           System.Environment
import           System.IO.Unsafe          (unsafePerformIO)

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Data.BEncode              as BE
import qualified Data.ByteString           as BS
import qualified Data.Map                  as M


hTorrent :: HTMonad ()
hTorrent = do
  -- Setup directory
  setupDirectory
  -- Parse torrent information
  parseMetaInfo
  -- Scan for previously downloaded pieces
  -- *Creates directory for file if new torrent
  scanPreviouslyDownloaded
  -- Try and connect to the announcer(s)
  -- This step also does the `connecting` and `announcing` protocol
  -- Also saves the peers obtained from the announcer
  gets _htsAnnouncers >>= connectToAnnounce
  -- After getting list of peers, perform peer wire protocol
  -- with each peer until every single piece is requested
  peerWireProtocol

initialState = HTState [] [] BS.empty socket BS.empty 0 Nothing "" M.empty M.empty
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
