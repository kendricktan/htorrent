module HTorrent.Tracker.UDP where


import           Data.Int
import           HTorrent.Types
import           HTorrent.Utils            (randomInt32, randomInteger)
import           Network.Socket            (Socket (..))
import           System.IO.Unsafe          (unsafePerformIO)

import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSBS

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


-- Information About Client --
--

btPeerId = unsafePerformIO $ randomInt32

btTransactionId :: Int32
btTransactionId = fromInteger . unsafePerformIO $ randomInt32


-- Handshake Handler
--
udpHandshake :: Socket -> IO (Either HTorrentError ())
udpHandshake s = do
  bytesSent <- NSBS.send s payload
  -- TODO: Validate Bytes Etc
  bytesRecv <- NSBS.recv s 2048
  let action = BSL.fromStrict $ BS.take 4 bytesRecv
      tx_id = BSL.fromStrict $ ((BS.drop 4) . (BS.take 8)) bytesRecv
      cnt_id = BSL.fromStrict $ BS.drop 8 bytesRecv
  print (Binary.decode action :: Int32)
  print (Binary.decode tx_id :: Int32)
  print (Binary.decode cnt_id :: Int64)
  return $ Right ()
  where p1 = Binary.encode btConnectionId
        p2 = Binary.encode btConnect
        p3 = Binary.encode btTransactionId
        payload = BS.concat (BSL.toStrict <$> [p1, p2, p3])
