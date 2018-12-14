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

import qualified Crypto.Hash.SHA1          as SHA1
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map.Strict           as M
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
      piecesList  = [(toInteger $ i `quot` 20, sliceBS i (i + 19) pieces) | i <- filter (\x -> x `rem` 20 == 0) [0..(BS.length pieces)]]
      piecesIndex = M.fromList piecesList :: M.Map Integer Hash
  -- Modify Pieces Index Map (A.K.A Dictionary)
  modify (\s -> s { _htsPiecesIndexHash = piecesIndex
                  , _htsAnnouncers      = announcers'
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

-- | How many pieces to download (based on request size)
--
getPiecesNo :: MetaInfo -> Integer
getPiecesNo m = getTotalLength m `quot` (fromIntegral btRequestSize)

pwpRequestPayload :: Integer -> Integer -> BS.ByteString
pwpRequestPayload pieceNo pieceOffset = btConstructPayload [p1, p2, p3, p4, p5]
  where p1 = Binary.encode (13 :: Word32)
        p2 = Binary.encode (6 :: Word8)
        p3 = Binary.encode (fromIntegral pieceNo :: Word32)
        p4 = Binary.encode (fromIntegral pieceOffset :: Word32)
        p5 = Binary.encode btRequestSize

-- The request socket might not necessarily be fulfilled
-- within a single request, hence we need a helper function
-- to help continually extract the pieces
pwpRequestPieceLen :: Integer -> BS.ByteString -> HTMonad BS.ByteString
pwpRequestPieceLen pieceLen bs = case BS.length bs < fromIntegral pieceLen of
                                   False -> return bs
                                   True -> do
                                     socket <- gets _htsConnectedSocket
                                     htSocketRecv socket (fromIntegral btRequestSize)
                                     bytesRecv <- gets _htsLastRecvBuffer
                                     pwpRequestPieceLen pieceLen (BS.append bs bytesRecv)

pwpRequestPiece :: Integer -> BS.ByteString -> Integer -> HTMonad ()
pwpRequestPiece i bs pieceNo = do
  mi <- asks _hteMetaInfo
  let collectedPieceSize = fromIntegral $ BS.length bs
      pieceSize          = (_tiPieceLength . _miInfo) mi
  liftIO $ putStrLn $ "Requesting piece " ++ (show pieceNo) ++ " got: " ++ (show $ BS.length bs) ++ ", want: " ++ (show pieceSize)
  case collectedPieceSize >= pieceSize of

    -- Haven't finished collecting
    False -> do
      socket <- gets _htsConnectedSocket
      -- Send payload
      htSocketSend socket (pwpRequestPayload pieceNo collectedPieceSize)
      -- Recv payload
      htSocketRecv socket (fromIntegral btRequestSize)
      -- Return ByteString
      bytesRecv <- gets _htsLastRecvBuffer
      -- Payload is bytes 13 onwards
      case BS.length bytesRecv >= 12 of
        -- TODO: Check if its been doing this for > 5 times or smthg
        False -> case i >= 5 of
                   True -> throwError $ InvalidRecvBytes "Requesting" BS.empty
                   False -> pwpRequestPiece (i+1) bs pieceNo
        True -> do
          let respLen     = fromIntegral (Binary.decode (BSL.fromStrict $ sliceBS 0 4 bytesRecv) :: Word32) - 9
              respId      = Binary.decode (BSL.fromStrict $ sliceBS 4 5 bytesRecv) :: Word8
              respIndex   = Binary.decode (BSL.fromStrict $ sliceBS 5 9 bytesRecv) :: Word32
              respBegin   = fromIntegral (Binary.decode (BSL.fromStrict $ sliceBS 9 13 bytesRecv) :: Word32)
              respPayload = sliceBS 13 (BS.length bytesRecv) bytesRecv
          case respId == 7 of
            False -> throwError InvalidRequestId
            True -> do
              case respBegin > BS.length bs of
                True -> case i >= 5 of
                          True -> throwError $ InvalidRecvBytes "Requesting" BS.empty
                          False -> pwpRequestPiece (i+1) bs pieceNo
                False -> do
                  bs' <- pwpRequestPieceLen respLen respPayload
                  pwpRequestPiece 0 (BS.append (BS.take respBegin bs) bs') pieceNo

    -- Finished collecting
    True -> do
      piecesIndexHash <- gets _htsPiecesIndexHash
      let piece                 = BS.take (fromIntegral pieceSize) bs
          pieceHash             = SHA1.hash piece
          maybeCorrectPieceHash = M.lookup pieceNo piecesIndexHash
      -- Check Hash
      case (== pieceHash) <$> maybeCorrectPieceHash of
        Nothing    -> do
          liftIO $ putStrLn "InvalidPiecesIndexLookup"
          throwError $ InvalidPiecesIndexLookup (fromIntegral pieceNo)
        Just False -> let (Just cph) = maybeCorrectPieceHash
                       in do
                         liftIO $ putStrLn "InvalidHash!"
                         liftIO $ putStrLn $ "Valid hash: " ++ (show cph)
                         liftIO $ putStrLn $ "Your hash: " ++ (show pieceHash)
                         throwError $ InvalidInfoHash (fromIntegral pieceNo) cph pieceHash
        Just True  -> do
          liftIO $ putStrLn "Success!"
          -- Save File
          liftIO $ BS.writeFile ("piece" ++ (show pieceNo) ++ ".part") piece
          mdp <- gets _htsDownloadedPieces
          modify (\s -> s { _htsDownloadedPieces = M.insert (fromIntegral pieceNo) piece mdp })

pwpRequestAll :: HTMonad ()
pwpRequestAll = do
  m <- asks _hteMetaInfo
  let blocksNo = getPiecesNo m
  pwpRequestPiece 0 BS.empty 0
  -- foldl (\_ a -> pwpRequestPiece 0 BS.empty a) (return ()) [0..blocksNo]
  -- return ()


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
  pwpRequestAll `catchError` (const $ pwpHandler xs)
  liftIO $ putStrLn "Requested..."
  return ()

peerWireProtocol :: HTMonad ()
peerWireProtocol = gets _htsPeers >>= pwpHandler
