{-# LANGUAGE OverloadedStrings #-}

{-
   https://www.libtorrent.org/udp_tracker_protocol.html#actions
-}

module HTorrent.Tracker.UDP where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Int
import           Data.Word
import           HTorrent.IPC
import           HTorrent.Types
import           HTorrent.Utils
import           HTorrent.Version
import           Network.Socket            (Socket (..))

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

import qualified Crypto.Hash.SHA1          as SHA1
import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T

-- BitTorrent Protocol Constants (Magic Numbers) --
--
btConnectionId :: Int64
btConnectionId = 0x41727101980

btConnect :: Int32
btConnect = 0

btAnnounce :: Int32
btAnnounce = 1

btScrape :: Int32
btScrape = 2

btError :: Int32
btError = 3

-- Protocol Payloads
--
btConnectingPayload :: BS.ByteString
btConnectingPayload = btConstructPayload [p1, p2, p3]
  where p1 = Binary.encode btConnectionId
        p2 = Binary.encode btConnect
        p3 = Binary.encode btTransactionId

btAnnouncingPayload :: BS.ByteString -> MetaInfo -> BS.ByteString
btAnnouncingPayload cid m = btConstructPayload [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14]
  where p1  = BSL.fromStrict cid
        p2  = Binary.encode btAnnounce
        p3  = Binary.encode btTransactionId
        p4  = BSL.fromStrict $ getInfoHash m
        p5  = BSL.fromStrict btPeerId
        p6  = Binary.encode (0 :: Int64) -- Downloaded
        p7  = Binary.encode (fromInteger (getTotalLength m) :: Int64) -- Bytes Needed Left
        p8  = Binary.encode (0 :: Int64) -- Uploaded
        p9  = Binary.encode (2 :: Int32) -- Event
        p10 = Binary.encode (0 :: Word32) -- Ip
        p11 = Binary.encode (btKey :: Word32) -- Unique Key
        p12 = Binary.encode (-1 :: Int32) -- Num_want
        p13 = Binary.encode (80 :: Word16) -- Port
        p14 = Binary.encode (1 :: Word16) -- Extensions

-- Response Checker
--
validConnectingResp :: BS.ByteString -> Bool
validConnectingResp s = p1 == 0 && p2 == btTransactionId
  where p1 = (Binary.decode $ BSL.fromStrict $ sliceBS 0 4 s) :: Int32
        p2 = (Binary.decode $ BSL.fromStrict $ sliceBS 4 8 s) :: Int32


validAnnouncingResp :: BS.ByteString -> Bool
validAnnouncingResp s = p1 == 1 && p2 == btTransactionId
  where p1 = (Binary.decode $ BSL.fromStrict $ sliceBS 0 4 s) :: Int32
        p2 = (Binary.decode $ BSL.fromStrict $ sliceBS 4 8 s) :: Int32

-- Handlers
--
udpConnecting :: HTMonad ()
udpConnecting = do
  socket <- gets _htsConnectedSocket
  -- Send Connecting Payload
  htSocketSend socket btConnectingPayload
  -- Recv Bytes
  htSocketRecv socket 2048
  -- Get last received buffer
  bytesRecv <- gets _htsLastRecvBuffer
  -- Ensure received bytes adhere to the protocol
  case validConnectingResp bytesRecv of
    True  -> do
      -- Store connectionId
      modify (\s -> s { _htsConnectionId = sliceBS 8 16 bytesRecv } )
      return ()
    False -> throwError $ InvalidRecvBytes "Connecting" bytesRecv


udpAnnouncing :: HTMonad ()
udpAnnouncing = do
  socket <- gets _htsConnectedSocket
  cid <- gets _htsConnectionId
  m <- asks _hteMetaInfo
  let payload = btAnnouncingPayload cid m
  -- Send Announcement Payload
  htSocketSend socket payload
  -- Recv Bytes
  htSocketRecv socket 10240
  -- Get last received buffer
  bytesRecv <- gets _htsLastRecvBuffer
  case validAnnouncingResp bytesRecv of
    False -> throwError $ InvalidRecvBytes "Announcing" bytesRecv
    True  -> case BS.length bytesRecv > 20 of
               False -> throwError NoAvailablePeers
               True -> do
                 let peers = binDecodePeers (sliceBS 20 (BS.length bytesRecv) bytesRecv)
                 modify (\s -> s { _htsPeers = peers })
                 return ()
