module HTorrent.Tracker.UDP where


import           HTorrent.Utils            (randomInt32, randomInteger)
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

btConnectionId = 0x41727101980

-- Information About Client --
--

btPeerId = unsafePerformIO $ randomInt32
btTransactionId = unsafePerformIO $ randomInt32
