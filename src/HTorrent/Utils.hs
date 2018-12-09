module HTorrent.Utils where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Char            (toLower)
import           Data.Text            (Text (..))
import           Data.Word
import           HTorrent.Types
import           Network.Socket       (tupleToHostAddress)
import           Network.URI
import           System.Random

import qualified Crypto.Hash.SHA1     as SHA1
import qualified Data.BEncode         as BE
import qualified Data.Binary          as Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T

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


-- | Array/ByteString Slice
--
sliceBS :: Int -> Int -> ByteString -> ByteString
sliceBS start end = BS.take (end - start + 1) . BS.drop start

-- | RNG Helpers
--
randomInteger :: Integer -> Integer -> IO Integer
randomInteger min max = getStdRandom (randomR (min, max))

randomInt32 = randomInteger 1 2147483647
