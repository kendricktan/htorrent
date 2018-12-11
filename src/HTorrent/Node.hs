{-# LANGUAGE OverloadedStrings #-}

module HTorrent.Node where


import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.IPC
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket            (Socket (..))
import           System.Timeout

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.ByteString           (ByteString (..))

import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS


-- | Tracker / Announcer Stuff
--
connectToAnnounce :: [Text] -> HTMonad ()
connectToAnnounce []       = throwError NoAvailableAnnounce
connectToAnnounce (a : as) = do
  let handleErr ma = ma `catchError` (const $ connectToAnnounce as)
  -- Initiate Socket
  handleErr (newSocketFromAnnounce a) 
  -- Send `Connecting` to UDP Tracker
  handleErr udpConnecting
  -- Send `Announcing` to UDP Tracker
  handleErr udpAnnouncing


convertMetaInfo2State :: HTMonad ()
convertMetaInfo2State = do
  mi <- asks _hteMetaInfo
  let torrentinfo = _miInfo mi
      announce    = _miAnnounce mi
      announcers  = maybe [[announce]] ((++) [[announce]]) (_miAnnounceList mi)
      announcers' = concat announcers
      pieces      = _tiPieces (_miInfo mi)
      piecesList  = [(sliceBS i (i + 20) pieces, toInteger $ i `quot` 20) | i <- [0,20..(BS.length pieces)]]
      piecesIndex = M.fromList piecesList :: M.Map Hash Integer
  -- Modify Pieces Index Map (A.K.A Dictionary)
  modify (\s -> s { _htsPiecesIndex = piecesIndex
                  , _htsAnnouncers = announcers'
                  })
  -- Try and connect to one announcer
  connectToAnnounce announcers'


-- | PeerWireProtocol stuff
--
pwpHandshakePayload :: MetaInfo -> BS.ByteString
pwpHandshakePayload m = btConstructPayload [p1, p2, p3, p4, p5]
  where p1 = Binary.encode (19 :: Int8)
        p2 = binEncodeStr "BitTorrent protocol"
        p3 = Binary.encode (0 :: Int64)
        p4 = BSL.fromStrict $ getInfoHash m
        p5 = BSL.fromStrict btPeerId

connectToPeer :: [(IP, Port)] -> HTMonad ()
connectToPeer []                = throwError NoAvailablePeers
connectToPeer ((ip, port) : xs) = do
  liftIO $ putStrLn $ "Connecting to peer: " ++ ip ++ " " ++ port
  (newPeerSocket ip port) `catchError` (const $ connectToPeer xs)
  liftIO $ putStrLn $ "Connected to peer: " ++ ip ++ " " ++ port
  liftIO $ putStrLn $ "Attempting peer handshake: " ++ ip ++ " " ++ port
  pwpHandshake `catchError` (const $ connectToPeer xs)
  bytesRecv <- gets _htsLastRecvBuffer
  liftIO $ putStrLn $ "Handshake successful" 
  liftIO $ print bytesRecv
  return ()

pwpHandshake :: HTMonad ()
pwpHandshake = do
  mi <- asks _hteMetaInfo
  socket <- gets _htsConnectedSocket
  let handshakepayload = pwpHandshakePayload mi
  -- Send Payload
  htSocketSend socket handshakepayload
  htSocketRecv socket 2048

peerWireProtocol :: HTMonad ()
peerWireProtocol = do
  peers <- gets _htsPeers
  -- TODO: catchError and try get other peers
  connectToPeer peers
  return ()
