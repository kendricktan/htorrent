module HTorrent.IPC where


import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Char                (toLower)
import           Data.Int
import           Data.Text                (Text (..))
import           Data.Word
import           HTorrent.Types
import           HTorrent.Utils
import           HTorrent.Version
import           Network.Socket           (Socket (..))
import           System.IO.Error
import           System.IO.Unsafe         (unsafePerformIO)
import           System.Timeout           (timeout)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Network.Socket           as NS

-- Client Identity
--
btPeerId :: BS.ByteString
btPeerId = BSL.toStrict $ binEncodeStr s
  where s = "-HT" ++ (filter (/= '.') hVersion) ++ "-" ++ (show . unsafePerformIO $ randomInteger 100000000000 999999999999)

btTransactionId :: Int32
btTransactionId = fromInteger . unsafePerformIO $ randomInt32

btKey :: Word32
btKey = fromInteger . unsafePerformIO $ randomInt32

-- Helper functions
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
