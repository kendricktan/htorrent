{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base    (try)
import           Data.Either               (either)
import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.IPC
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket            (SockAddr (..), Socket (..))
import           System.Environment

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T


hTorrent :: MetaInfo -> IO ()
hTorrent m = do
  es <- newSocket m
  case es of
    Left err -> print $ "Error: " ++ show err
    Right s  -> do
      br <- udpConnecting s
      case br of
        Left err -> print err
        Right br' -> do
          (udpAnnouncing (sliceBS 8 16 br') m) s
          return ()


main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile torFile
  let decoded = BE.decode contents :: BE.Result MetaInfo
  case decoded of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right v -> hTorrent v
