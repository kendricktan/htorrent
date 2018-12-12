module HTorrent.Utils where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Trans
import           Data.ByteString           (ByteString)
import           Data.Char                 (toLower)
import           Data.Text                 (Text (..))
import           Data.Word
import           HTorrent.Types
import           Network.Socket            (tupleToHostAddress)
import           Network.Socket            (Socket (..))
import           Network.URI
import           System.Random
import           System.Timeout            (timeout)

import qualified Crypto.Hash.SHA1          as SHA1
import qualified Data.BEncode              as BE
import qualified Data.Binary               as Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

-- | Parse Announce To Scheme (UDP/HTTP), Host, Port
--
parseAnnounce :: Text -> Maybe (Scheme, Host, Port)
parseAnnounce t = liftA3 (,,) (((<$>) toLower) <$> s) h p
  where uri = parseURI $ T.unpack t
        s = filter (/= ':') <$> uriScheme <$> uri
        uriA = uri >>= uriAuthority
        h = uriRegName <$> uriA
        p = (filter (/= ':')) <$> uriPort <$> uriA

-- | Computes Info Hash of the given Torrent File
--
getInfoHash :: MetaInfo -> BS.ByteString
getInfoHash m = SHA1.hash $ BSL.toStrict $ BE.encode (_miInfo m)

-- | Encodes String Into Binary Format
--
binEncodeStr :: String -> BSL.ByteString
binEncodeStr s = foldl f (BSL.empty) s
  where f = (\b a -> BSL.append b (Binary.encode a))

-- | Gets Host IP Address
--
binDecodeIP :: BS.ByteString -> IP
binDecodeIP b = foldr (\a b -> a ++ if b == "" then b else "." ++ b) "" hs
  where f s e = (show . toInteger) (Binary.decode (BSL.fromStrict $ sliceBS s e b) :: Word8)
        hs = [f i (i+1) | i <- [0..3]]

binDecodePort :: BS.ByteString -> Port
binDecodePort p = (show . toInteger) (Binary.decode (BSL.fromStrict p) :: Word16)

-- | Decodes Peers ByteString information
binDecodePeers :: BS.ByteString -> [(IP, Port)]
binDecodePeers b = (\(h, p) -> (binDecodeIP h, binDecodePort p)) <$> b'
  where b' = [ (sliceBS i (i+4) b, sliceBS (i+4) (i+6) b) | i <- [0,6..(BS.length b)-6]]

-- | Is Single or Multiple Files
--
isSingleFile :: MetaInfo -> Bool
isSingleFile m = case _tiLength (_miInfo m) of
                   Just _  -> True
                   Nothing -> False

getTotalLength :: MetaInfo -> Integer
getTotalLength m = case isSingleFile m of
                     True  -> let (Just l) = _tiLength (_miInfo m) in l
                     False -> let (Just files) = _tiFiles (_miInfo m)
                               in foldl (\b a -> b + (_fiLength a)) 0 files

-- | Payload Construction
--
btConstructPayload :: [BSL.ByteString] -> BS.ByteString
btConstructPayload b = BS.concat $ BSL.toStrict <$> b

-- | Socket helpers (with try and timeout)
--
defaultTimeout = 1000000

htSocketConnect :: Socket -> NS.SockAddr -> HTMonad ()
htSocketConnect socket a = do
  maybeSocket <- liftIO $ timeout defaultTimeout $ (try $ NS.connect socket a :: IO (Either IOError ()))
  case maybeSocket of
    Nothing -> throwError SocketConnectTimeout
    Just e  -> case e of
                 Left err -> throwError SocketConnectError
                 Right _  -> do
                   modify (\s -> s { _htsConnectedSocket = socket })
                   return ()


htSocketSend :: Socket -> BS.ByteString -> HTMonad ()
htSocketSend s bs = do
  maybeSent <- liftIO $ timeout defaultTimeout $ (try $ NSBS.send s bs :: IO (Either IOError Int))
  case maybeSent of
    Nothing                   -> throwError SocketSendTimeout
    Just eitherErrOrBytesSent -> case eitherErrOrBytesSent of
                                   Left _            -> throwError SocketSendError
                                   Right bytesSentNo -> case bytesSentNo == BS.length bs of
                                                          True  -> return ()
                                                          False -> throwError $ SocketSendInvalidLength (BS.length bs) bytesSentNo

htSocketRecv :: Socket -> Int -> HTMonad ()
htSocketRecv s i = do
  maybeRecv <- liftIO $ timeout defaultTimeout $ (try $ NSBS.recv s i :: IO (Either IOError BS.ByteString))
  case maybeRecv of
    Nothing                   -> throwError SocketRecvTimeout
    Just eitherErrOrBytesRecv -> case eitherErrOrBytesRecv of
                                   Left _          -> throwError SocketRecvError
                                   Right bytesRecv -> do
                                     modify (\s -> s { _htsLastRecvBuffer = bytesRecv } )
                                     return ()
-- Socket Helper functions
--
newSocketFromAnnounce :: Text -> HTMonad ()
newSocketFromAnnounce a = case parseAnnounce a of
  Nothing -> throwError (InvalidAnnounce a)
  Just (s, h, p) -> do
    socket <- liftIO $ NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    -- Get socket destination info
    sa <- liftIO $ NS.getAddrInfo (Just NS.defaultHints) (Just h) (Just p)
    -- Connect socket to address
    htSocketConnect socket (NS.addrAddress $ head sa)

newPeerSocket :: IP -> Port -> HTMonad ()
newPeerSocket h p = do
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_NUMERICHOST, NS.AI_NUMERICSERV], NS.addrSocketType = NS.Stream }
  -- Net socket
  socket <- liftIO $ NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  -- Get socket destination info
  sa <- liftIO $ NS.getAddrInfo (Just hints) (Just h) (Just p)
  -- Try and connect socket
  htSocketConnect socket (NS.addrAddress $ head sa)

-- Release Socket if Exception Occurs
--
runSocket :: Socket -> (Socket -> IO c) -> IO c
runSocket socket = bracket (return socket) NS.close

-- | Array/ByteString Slice
--
sliceBS :: Int -> Int -> ByteString -> ByteString
sliceBS start end = BS.take (end - start + 1) . BS.drop start

-- | Bin 2 Binary
--
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

-- | RNG Helpers
--
randomInteger :: Integer -> Integer -> IO Integer
randomInteger min max = getStdRandom (randomR (min, max))

randomInt32 = randomInteger 1 2147483647
