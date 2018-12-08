module HTorrent.IPC where


import           Control.Exception
import           Data.Char         (toLower)
import           Data.Text         (Text (..))
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket    (Socket (..))
import           System.Timeout    (timeout)

import qualified Network.Socket    as NS

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

-- Release Socket if Exception Occurs
--
runSocket :: Socket -> (Socket -> IO c) -> IO c
runSocket socket = bracket (return socket) NS.close
