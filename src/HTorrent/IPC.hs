module HTorrent.IPC where


import           Control.Exception
import           Control.Exception
import           Data.Char            (toLower)
import           Data.Int
import           Data.Text            (Text (..))
import           Data.Word
import           HTorrent.Types
import           HTorrent.Utils
import           HTorrent.Version
import           Network.Socket       (Socket (..))
import           System.IO.Error
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Timeout       (timeout)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Socket       as NS

-- Client Identity
--
btPeerId :: BS.ByteString
btPeerId = BSL.toStrict $ binEncodeStr s
  where s = "-HT" ++ (filter (/= '.') hVersion) ++ "-" ++ (show . unsafePerformIO $ randomInteger 100000000000 999999999999)

btTransactionId :: Int32
btTransactionId = fromInteger . unsafePerformIO $ randomInt32

btKey :: Word32
btKey = fromInteger . unsafePerformIO $ randomInt32

-- Defaults (Microseconds)
--
defaultTimeout = 500000

-- Helper functions
--
newSocketFromAnnounce :: Text -> IO (Either HTError Socket)
newSocketFromAnnounce a = case parseAnnounce a of
  Nothing -> return $ Left (InvalidAnnounce a)
  Just (s, h, p) -> do
    -- UDP or TCP
    let protocol = if s == "udp" then NS.Datagram else NS.Stream
    -- Net socket
    s <- NS.socket NS.AF_INET protocol NS.defaultProtocol
    -- Get socket destination info
    sa <- NS.getAddrInfo (Just NS.defaultHints) (Just h) (Just p)
    -- Connect socket to address (0.5 sec connect time)
    sa' <- timeout defaultTimeout $ NS.connect s (NS.addrAddress $ head sa)
    case sa' of
      Just _  -> return (Right s)
      Nothing -> return $ Left (NoResponse h p)

newPeerSocket :: IP -> Port -> IO (Either HTError Socket)
newPeerSocket h p = do
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_NUMERICHOST, NS.AI_NUMERICSERV], NS.addrSocketType = NS.Stream } 
  -- Net socket
  s <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  -- Get socket destination info
  sa <- NS.getAddrInfo (Just hints) (Just h) (Just p)
  -- Try and connect socket
  sa' <- timeout defaultTimeout $ (try $ NS.connect s (NS.addrAddress $ head sa) :: IO (Either IOError ()))
  case sa' of
    Just e  -> case e of
                 Left err -> return $ Left (NoResponse h p)
                 Right _  -> return (Right s)
    Nothing -> return $ Left (NoResponse h p)

-- Release Socket if Exception Occurs
--
runSocket :: Socket -> (Socket -> IO c) -> IO c
runSocket socket = bracket (return socket) NS.close
