{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base    (try)
import           Data.Either               (either)
import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket
import           Network.Socket            (SockAddr (..), Socket (..))
import           System.Environment

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T

connectConst = 0

connectionIdConst = 0x41727101980

getSockAddrFromAnnounce :: Text -> IO SockAddr
getSockAddrFromAnnounce t = do
  sockaddrs <- NS.getAddrInfo (Just NS.defaultHints) (Just host) (Just port)
  return $ (NS.addrAddress . head) sockaddrs
  where t' = (T.replace "//" "") <$> T.split (== ':') t
        host = (T.unpack . head . tail) t'
        port = T.unpack . head $ T.split (== '/') (last t')

sendHandshake :: MetaInfo -> Socket -> IO ()
sendHandshake m s = do
  let h1 = BSL.toStrict $ Binary.encode (connectionIdConst :: Int64)
      h2 = BSL.toStrict $ Binary.encode (connectConst :: Int32)
      h3 = BSL.toStrict $ Binary.encode (861058307 :: Int32)
      payload = BS.concat [h1, h2, h3]
  sockaddr <- getSockAddrFromAnnounce (_miAnnounce m)
  bytesent <- NSBS.sendTo s payload sockaddr
  putStrLn $ "Bytes sent: " ++ show bytesent
  return ()


main :: IO ()
main = do
  (torFile : _) <- getArgs
  contents <- BS.readFile torFile
  let decoded = BE.decode contents :: BE.Result MetaInfo
  case decoded of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right v -> do
      socket <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol -- UDP = Datagram
      sendHandshake v socket
      (resp, from) <- NSBS.recvFrom socket 2048
      let action = BSL.fromStrict $ BS.take 4 resp
          tx_id = BSL.fromStrict $ ((BS.drop 4) . (BS.take 8)) resp
          cnt_id = BSL.fromStrict $ BS.drop 8 resp
      print (Binary.decode action :: Int32)
      print (Binary.decode tx_id :: Int32)
      print (Binary.decode cnt_id :: Int64)
