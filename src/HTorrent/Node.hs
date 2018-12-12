{-# LANGUAGE OverloadedStrings #-}

module HTorrent.Node where


import           Data.Int
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.Types
import           HTorrent.Utils
import           HTorrent.Version
import           Network.Socket            (Socket (..))
import           System.IO.Unsafe          (unsafePerformIO)
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


-- Node Identity
--
btPeerId :: BS.ByteString
btPeerId = BSL.toStrict $ binEncodeStr s
  where s = "-HT" ++ (filter (/= '.') hVersion) ++ "-" ++ (show . unsafePerformIO $ randomInteger 100000000000 999999999999)

btTransactionId :: Int32
btTransactionId = fromInteger . unsafePerformIO $ randomInt32

btKey :: Word32
btKey = fromInteger . unsafePerformIO $ randomInt32

btRequestSize :: Word32
btRequestSize = 2^14

-- | Given MetaInfo (Torrent file), parse the information
-- into the state monad
--
parseMetaInfo :: HTMonad ()
parseMetaInfo = do
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

---- | PeerWireProtocol | ----
------------------------------

-- | Handshake (bitfield is sent along with handshake usually)
--
validHandshakeResp :: MetaInfo -> BS.ByteString -> Bool
validHandshakeResp m bs = if BS.length bs >= 68
    then b1 && b2 && b3
    else False
      where b1 = ((Binary.decode $ BSL.fromStrict $ sliceBS 0 1 bs) :: Int8) == 19
            -- Should be 20 bytes, but the additional byte
            -- Contains some useless information
            b2 = sliceBS 1 19 bs == "BitTorrent protocol"
            b3 = sliceBS 28 47 bs == getInfoHash m

pwpHandshakePayload :: MetaInfo -> BS.ByteString
pwpHandshakePayload m = btConstructPayload [p1, p2, p3, p4, p5]
  where p1 = Binary.encode (19 :: Int8)
        p2 = binEncodeStr "BitTorrent protocol"
        p3 = Binary.encode (0 :: Int64)
        p4 = BSL.fromStrict $ getInfoHash m
        p5 = BSL.fromStrict btPeerId

parseBitfieldResp :: BS.ByteString -> HTMonad ()
parseBitfieldResp bs = case BS.length bs > 5 of
                         False -> throwError InvalidPeerBitfield
                         True -> do
                           let f x     = Binary.decode (BSL.fromStrict x)
                               len     = fromIntegral (f (sliceBS 0 4 bs) :: Word32) - 1
                               bid     = f (sliceBS 4 5 bs) :: Word8
                           if bid /= 5
                              then throwError InvalidPeerBitfield
                              else do
                                let fields   = BS.drop 5 bs
                                    fields'  = [ f ((sliceBS i (i+1)) fields) :: Word8 | i <- [0..(BS.length fields - 1)] ]
                                    fields'' = Prelude.concat $ (toBinary . fromIntegral) <$> fields'
                                modify (\s -> s { _htsPeerBitfield = Just fields'' })
                                return ()

pwpHandshake :: HTMonad ()
pwpHandshake = do
  mi <- asks _hteMetaInfo
  socket <- gets _htsConnectedSocket
  -- Send Payload
  htSocketSend socket (pwpHandshakePayload mi)
  htSocketRecv socket 10240
  -- Recv Payload
  bytesRecv <- gets _htsLastRecvBuffer
  let handshakeResp = sliceBS 0 68 bytesRecv
      bitfieldResp  = BS.drop 68 bytesRecv
  case validHandshakeResp mi handshakeResp of
    False -> throwError InvalidPeerHandshake
    True  -> do
      -- Ignore errors thrown by bitfield as
      -- Not all clients will return bitfield data
      (parseBitfieldResp bitfieldResp) `catchError` (const $ modify (\s -> s { _htsPeerBitfield = Nothing } ))
      return ()


-- | Bitfield
--
validBitfieldResp :: BS.ByteString -> Bool
validBitfieldResp bs = undefined
  where p1 = (Binary.decode $ BSL.fromStrict $ sliceBS 0 4 bs) :: Int32
        p2 = (Binary.decode $ BSL.fromStrict $ sliceBS 4 5 bs) :: Int8


-- | Interested
--
pwpInterestedPayload :: BS.ByteString
pwpInterestedPayload = btConstructPayload [p1, p2]
  where p1 = Binary.encode (1 :: Int32)
        p2 = Binary.encode (2 :: Int8)

pwpInterested :: HTMonad()
pwpInterested = do
  socket <- gets _htsConnectedSocket
  -- Send payload
  htSocketSend socket pwpInterestedPayload
  -- Recv payload
  htSocketRecv socket 1024

-- | Request
--

-- | How many blocks to download (based on request size)
--
getBlocksNo :: MetaInfo -> Integer
getBlocksNo m = getTotalLength m `quot` (fromIntegral btRequestSize)

pwpRequestPayload :: Int -> Int -> BS.ByteString
pwpRequestPayload pieceNo pieceOffset = btConstructPayload [p1, p2, p3, p4, p5]
  where p1 = Binary.encode (13 :: Word32)
        p2 = Binary.encode (6 :: Int8)
        p3 = Binary.encode (fromIntegral pieceNo :: Word32)
        p4 = Binary.encode (fromIntegral pieceOffset :: Word32)
        p5 = Binary.encode btRequestSize

pwpRequestPiece :: Int -> Int -> HTMonad ()
pwpRequestPiece pieceNo pieceOffset = do
  socket <- gets _htsConnectedSocket
  -- Send payload
  htSocketSend socket (pwpRequestPayload pieceNo pieceOffset)
  -- Recv payload
  htSocketRecv socket (fromIntegral btRequestSize)
  gets _htsLastRecvBuffer >>= liftIO . print
  return ()

pwpRequestAll :: HTMonad ()
pwpRequestAll = do
  m <- asks _hteMetaInfo
  let blocksNo = getBlocksNo m
  undefined


-- | Helper function to connect to peer
--
pwpHandler :: [(IP, Port)] -> HTMonad ()
pwpHandler []                = throwError NoAvailablePeers
pwpHandler ((ip, port) : xs) = do
  (newPeerSocket ip port) `catchError` (const $ pwpHandler xs)
  liftIO $ putStrLn "Connecting..."
  pwpHandshake `catchError` (const $ pwpHandler xs)
  liftIO $ putStrLn "Connected..."
  pwpInterested `catchError` (const $ pwpHandler xs)
  liftIO $ putStrLn "Interested..."
  (pwpRequestPiece 0 0) `catchError` (const $ pwpHandler xs)
  liftIO $ putStrLn "Requested..."
  return ()

peerWireProtocol :: HTMonad ()
peerWireProtocol = gets _htsPeers >>= pwpHandler
