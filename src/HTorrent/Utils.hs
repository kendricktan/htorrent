module HTorrent.Utils where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Char            (toLower)
import           Data.Text            (Text (..))
import           HTorrent.Types
import           Network.URI
import           System.Random

import qualified Crypto.Hash.SHA1     as SHA1
import qualified Data.BEncode         as BE
import qualified Data.Binary          as Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T

type Host = String
type Port = String
type Scheme = String

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

-- | Array/ByteString Slice
--
sliceBS :: Int -> Int -> ByteString -> ByteString
sliceBS start end = BS.take (end - start + 1) . BS.drop start

-- | RNG Helpers
--
randomInteger :: Integer -> Integer -> IO Integer
randomInteger min max = getStdRandom (randomR (min, max))

randomInt32 = randomInteger 1 2147483647
