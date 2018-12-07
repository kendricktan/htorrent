{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HTorrent.Types where

import           GHC.Generics

import           Data.BEncode
import           Data.ByteString (ByteString (..))
import           Data.Maybe      (Maybe (..))
import           Data.Text       (Text (..))


type Host = String
type Port = String
type Scheme = String


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
