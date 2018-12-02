module Main where

import           HTorrent.Types
import           System.Environment

import           Control.Exception.Base (try)

import qualified Data.BEncode           as BE
import qualified Data.ByteString        as BS

main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile "ubuntu1804.torrent"
  let m = BE.decode contents :: BE.Result MetaInfo
  undefined
