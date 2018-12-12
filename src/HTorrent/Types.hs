{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module HTorrent.Types where

import           Data.BEncode
import           Data.ByteString          (ByteString (..))
import           Data.Map.Lazy            (Map (..))
import           Data.Maybe               (Maybe (..))
import           Data.Text                (Text (..))
import           Data.Word
import           GHC.Generics
import           Network.Socket           (HostAddress (..), Socket (..))

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy


-- | Aliases
--

type Hash   = ByteString
type IP     = String
type Host   = String
type Port   = String
type Scheme = String


-- | Env & State
--
data HTEnv = HTEnv
  { _hteMetaInfo     :: MetaInfo
  } deriving Show


data HTState = HTState
  { _htsAnnouncers           :: [Text]
  , _htsPeers                :: [(IP, Port)]
  , _htsConnectionId         :: ByteString
  , _htsConnectedSocket      :: Socket
  , _htsLastRecvBuffer       :: ByteString
  , _htsTotalDownloadedBytes :: Integer
  , _htsPeerBitfield         :: Maybe [Int]
  , _htsHasDownloadedPieces  :: Map Integer Bool
  , _htsDownloadedPieces     :: Map Integer ByteString
  , _htsPiecesIndex          :: Map Hash Integer
  } deriving Show


data HTError = InvalidAnnounce Text
             | InvalidRecvBytes Text ByteString
             | NoTrackerResponse
             | NoPeerResponse
             | NoAvailablePeers
             | NoAvailableAnnounce
             | SocketSendTimeout
             | SocketSendError
             | SocketSendInvalidLength Int Int -- Expected, Sent
             | SocketRecvTimeout
             | SocketRecvError
             | SocketConnectTimeout
             | SocketConnectError
             | InvalidPeerHandshake
             | InvalidPeerBitfield
             | UnknownError
             deriving (Show, Generic)


newtype HTMonad a = HTMonad (ReaderT HTEnv (StateT HTState (ExceptT HTError IO)) a)
   deriving (Applicative, Monad, MonadState HTState, MonadReader HTEnv, MonadError HTError, Functor, MonadIO)


runHTMonad :: HTMonad a -> HTEnv -> HTState -> IO (Either HTError a)
runHTMonad (HTMonad a) env state = runExceptT sout
  where rout = runReaderT a env
        sout = evalStateT rout state


-- | Primitives
--
data FilesInfo = FilesInfo
  { _fiLength :: Integer
  , _fiMd5sum :: Maybe ByteString
  , _fiPath   :: [Text]
  } deriving (Generic, Show, Read, Eq)

instance BEncode FilesInfo where
  toBEncode FilesInfo {..} = toDict $
       "length" .=! _fiLength
    .: "md5sum" .=? _fiMd5sum
    .: "path"   .=! _fiPath
    .: endDict

  fromBEncode = fromDict $ do
    FilesInfo <$>! "length"
              <*>? "md5sum"
              <*>! "path"

data TorrentInfo = TorrentInfo
  { _tiFiles       :: Maybe [FilesInfo]
  , _tiLength      :: Maybe Integer
  , _tiMd5sum      :: Maybe ByteString
  , _tiName        :: Text
  , _tiPieceLength :: Integer
  , _tiPieces      :: ByteString
  , _tiPrivate     :: Maybe Integer
  } deriving (Generic, Show, Read, Eq)

instance BEncode TorrentInfo where
  toBEncode TorrentInfo {..} = toDict $
       "files"        .=? _tiFiles
    .: "length"       .=? _tiLength
    .: "md5sum"       .=? _tiMd5sum
    .: "name"         .=! _tiName
    .: "piece length" .=! _tiPieceLength
    .: "pieces"       .=! _tiPieces
    .: "private"      .=? _tiPrivate
    .: endDict

  fromBEncode = fromDict $ do
    TorrentInfo <$>? "files"
                <*>? "length"
                <*>? "md5sum"
                <*>! "name"
                <*>! "piece length"
                <*>! "pieces"
                <*>? "private"

data MetaInfo = MetaInfo
  { _miAnnounce     :: Text
  , _miAnnounceList :: Maybe [[Text]]
  , _miComment      :: Maybe Text
  , _miCreatedBy    :: Maybe Text
  , _miCreation     :: Maybe Integer
  , _miEncoding     :: Maybe Text
  , _miInfo         :: TorrentInfo
  } deriving (Generic, Show, Eq, Read)

instance BEncode MetaInfo where
  toBEncode MetaInfo {..} = toDict $
       "announce"      .=! _miAnnounce
    .: "announce-list" .=? _miAnnounceList
    .: "comment"       .=? _miComment
    .: "created by"    .=? _miCreatedBy
    .: "creation date" .=? _miCreation
    .: "encoding"      .=? _miEncoding
    .: "info"          .=! _miInfo
    .: endDict

  fromBEncode = fromDict $ do
    MetaInfo <$>! "announce"
             <*>? "announce-list"
             <*>? "comment"
             <*>? "created by"
             <*>? "creation date"
             <*>? "encoding"
             <*>! "info"
