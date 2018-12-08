{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base    (try)
import           Data.Either               (either)
import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.IPC
import HTorrent.Node
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket            (SockAddr (..), Socket (..))
import           System.Environment
import           System.Timeout            (timeout)

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import qualified Data.Text                 as T


hTorrent :: MetaInfo -> IO ()
hTorrent m = do
  maybepeers <- runHTMonad udpTracker env state
  print maybepeers
  where state = HTState [] Nothing 0 M.empty M.empty
        env   = HTEnv m

--   es <- newSocket m
--   case es of
--     Left err -> print $ "Error: " ++ show err
--     Right s  -> do
--       br <- udpConnecting s
--       case br of
--         Left err -> print err
--         Right br' -> do
--           ab <- (udpAnnouncing (sliceBS 8 16 br') m) s
--           case ab of
--             Left err -> print $ "Error: " ++ show err
--             Right ab' -> do
--               let peerips = binDecodePeers (sliceBS 20 (BS.length ab') ab') -- Response starts after 20 bytes
--               print peerips
--

main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile torFile
  let decoded = BE.decode contents :: BE.Result MetaInfo
  case decoded of
    Left e  -> putStrLn $ "Torrent File Read Error: " ++ show e
    Right v -> hTorrent v
