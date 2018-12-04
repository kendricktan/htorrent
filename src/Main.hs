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


main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile torFile
  let decoded = BE.decode contents :: BE.Result MetaInfo
  case decoded of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right v -> do
      e <- newSocket v
      either undefined (\s -> runSocket s udpHandshake) e
      return ()
