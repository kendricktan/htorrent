{-# LANGUAGE OverloadedStrings #-}

module HTorrent.Node where


import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.IPC
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           System.Timeout

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.ByteString           (ByteString (..))

import qualified Network.Socket as NS
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Network.Socket.ByteString as NSBS


-- | Tracker / Announcer Stuff
--

-- | Given a list of announcers, try to:
-- 1. Connect to them at the specified host & port
-- 2. Initiate `Connecting` and `Announcing`
-- 3. Get peers from `Announcing`
-- 4. If above is successful, then update the state within
--
-- Specifications: https://www.libtorrent.org/udp_tracker_protocol.html
--
connectToAnnounce :: [Text] -> HTMonad ()
connectToAnnounce []       = return ()
connectToAnnounce (a : as) = do
  liftIO $ putStrLn $ "Trying to connect to Announce: " ++ (T.unpack a)
  -- Attempt to get new socket
  es <- liftIO $ newSocketFromAnnounce a
  case es of
    Left err -> do
      liftIO $ print err
      connectToAnnounce as
    Right s  -> do
      -- `Connecting`
      ec <- liftIO $ udpConnecting s
      case ec of
        Left err' -> do
          liftIO $ putStrLn $ "Connecting received invalid response: " ++ (T.unpack a)
          connectToAnnounce as
        -- Need to retrieve connectionId (Bytes 8-16)
        -- from received bytestring
        Right bc -> do
          let connectionId = sliceBS 8 16 bc
          mi <- asks _hteMetaInfo
          ea <- liftIO $ (udpAnnouncing (sliceBS 8 16 bc) mi) s
          -- `Announcing`
          case ea of
            Left err'' -> do
              liftIO $ putStrLn $ "Announcing received invalid response: " ++ (T.unpack a)
              connectToAnnounce as
            Right ba   -> case BS.length ba > 20 of
                            False -> do
                              liftIO $ putStrLn $ "No peers received from: " ++ (T.unpack a)
                              connectToAnnounce as
                            True -> do
                              let peers        = binDecodePeers (sliceBS 20 (BS.length ba) ba)
                              -- Update state
                              modify (\s' -> s' { _htsPeers            = peers
                                                , _htsConnectionId     = Just connectionId
                                                , _htsCurrentConnected = Just s
                                                })
                              liftIO $ putStrLn $ "Connected to: " ++ (T.unpack a)
                              liftIO $ putStrLn $ "Number of peers received: " ++ (show $ Prelude.length peers)


initUdpTracker :: HTMonad ()
initUdpTracker = do
  mi <- asks _hteMetaInfo
  let torrentinfo = _miInfo mi
      announce    = _miAnnounce mi
      announcers  = maybe [[announce]] ((++) [[announce]]) (_miAnnounceList mi)
      announcers' = concat announcers
      pieces      = _tiPieces (_miInfo mi)
      piecesList  = [(sliceBS i (i + 20) pieces, toInteger $ i `quot` 20) | i <- [0,20..(BS.length pieces)]]
      piecesIndex = M.fromList piecesList :: M.Map Hash Integer
  -- Modify Pieces Index Map (A.K.A Dictionary)
  modify (\s -> s { _htsPiecesIndex = piecesIndex })
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
connectToPeer []                = return ()
connectToPeer ((ip, port) : xs) = do
  ms <- liftIO $ newPeerSocket ip port
  case ms of
    Left err -> do
      liftIO $ print err
      connectToPeer xs
    Right socket -> do
      -- Send handshake
      mi <- asks _hteMetaInfo
      let handshakepayload = pwpHandshakePayload mi
      -- Sends payload
      bytesSent <- liftIO $ timeout 500000 $ NSBS.send socket handshakepayload
      case bytesSent of
        Nothing -> do
          liftIO $ putStrLn $ "Socket SEND timeout: " ++ ip ++ " " ++ port
          connectToPeer xs
        Just i -> do
          bytesRecv <- liftIO $ timeout 500000 $ NSBS.recv socket 2048
          case bytesRecv of
            Nothing -> do
              liftIO $ putStrLn $ "Socket RECV timeout: " ++ ip ++ " " ++ port
              connectToPeer xs
            Just b -> do
              liftIO $ putStrLn "Success!"
              liftIO $ print b
              connectToPeer xs

initPeerWireProtocol :: HTMonad ()
initPeerWireProtocol = do
  peers <- gets _htsPeers
  connectToPeer peers
