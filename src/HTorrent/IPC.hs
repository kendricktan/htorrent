module HTorrent.IPC where


import           Control.Exception
import           Data.Char         (toLower)
import           HTorrent.Types
import           HTorrent.Utils
import           Network.Socket    (Socket (..))

import qualified Network.Socket    as NS

-- Defaults
--
defaultRecvTimeout = 500
defaultSendTimeout = 500

-- Helper functions
--
newSocket :: MetaInfo -> IO (Either HTorrentError Socket)
newSocket m = case parseAnnounce (_miAnnounce m) of
  Nothing -> return $ Left (InvalidAnnounce $ _miAnnounce m)
  Just (s, h, p) -> do
    -- UDP or TCP
    let protocol = if s == "udp" then NS.Datagram else NS.Stream
    -- Net socket
    s <- NS.socket NS.AF_INET protocol NS.defaultProtocol
    -- Set socket options
    -- setSocketRecvTimeout s defaultRecvTimeout
    -- setSocketSendTimeout s defaultSendTimeout
    -- Get socket destination info
    sa <- NS.getAddrInfo (Just NS.defaultHints) (Just h) (Just p)
    -- Connect socket to address
    NS.connect s (NS.addrAddress $ head sa)
    return (Right s)

setSocketRecvTimeout :: Socket -> Int -> IO ()
setSocketRecvTimeout s = NS.setSocketOption s NS.RecvTimeOut

setSocketSendTimeout :: Socket -> Int -> IO ()
setSocketSendTimeout s = NS.setSocketOption s NS.SendTimeOut

-- Release Socket if Exception Occurs
-- TODO: Logging
runSocket :: Socket -> (Socket -> IO c) -> IO c
runSocket socket = bracket (return socket) NS.close
