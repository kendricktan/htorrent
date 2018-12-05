module HTorrent.Errors where


import           Data.ByteString (ByteString (..))
import           Data.Text


data HTorrentError = InvalidAnnounce Text
                   | InvalidRecvBytes Text ByteString deriving Show
