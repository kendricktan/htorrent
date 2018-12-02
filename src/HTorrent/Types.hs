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


data SingleFileInfo = SingleFile
  { _sfLength      :: Integer
  , _sfMd5sum      :: Maybe ByteString
  , _sfName        :: Text
  , _sfPieceLength :: Integer
  , _sfPieces      :: ByteString
  , _sfPrivate     :: Maybe Integer
  } deriving (Generic, Show, Read, Eq)


instance BEncode SingleFileInfo where
  toBEncode SingleFile {..} = toDict $
       "length"       .=! _sfLength
    .: "md5sum"       .=? _sfMd5sum
    .: "name"         .=! _sfName
    .: "piece length" .=! _sfPieceLength
    .: "pieces"       .=! _sfPieces
    .: "private"      .=? _sfPrivate
    .: endDict

  fromBEncode = fromDict $ do
    SingleFile <$>! "length"
               <*>? "md5sum"
               <*>! "name"
               <*>! "piece length"
               <*>! "pieces"
               <*>? "private"

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

data MultiFileInfo = MultiFileInfo
  { _mfFiles       :: [FilesInfo]
  , _mfName        :: Text
  , _mfPieceLength :: Integer
  , _mfPieces      :: ByteString
  } deriving (Generic, Show, Read, Eq)

instance BEncode MultiFileInfo where
  toBEncode MultiFileInfo {..} = toDict $
       "files"        .=! _mfFiles
    .: "name"         .=! _mfName
    .: "piece length" .=! _mfPieceLength
    .: "pieces"       .=! _mfPieces
    .: endDict

  fromBEncode = fromDict $ do
    MultiFileInfo <$>! "files"
                  <*>! "name"
                  <*>! "piece length"
                  <*>! "pieces"

data MetaInfo = MetaInfo
  { _miAnnounce     :: Text
  , _miAnnounceList :: Maybe [[Text]]
  , _miComment      :: Maybe Text
  , _miCreatedBy    :: Maybe Text
  , _miCreation     :: Maybe Integer
  , _miEncoding     :: Maybe Text
  , _mimInfo        :: Maybe MultiFileInfo  -- MetaInfo Multi
  , _misInfo        :: Maybe SingleFileInfo -- MetaInfo Single
  } deriving (Generic, Show, Eq, Read)

instance BEncode MetaInfo where
  toBEncode MetaInfo {..} = toDict $
       "announce"      .=! _miAnnounce
    .: "announce-list" .=? _miAnnounceList
    .: "comment"       .=? _miComment
    .: "created by"    .=? _miCreatedBy
    .: "creation date" .=? _miCreation
    .: "encoding"      .=? _miEncoding
    .: "info"          .=? _mimInfo
    .: "info"          .=? _misInfo
    .: endDict

  fromBEncode = fromDict $ do
    MetaInfo <$>! "announce"
             <*>? "announce-list"
             <*>? "comment"
             <*>? "created by"
             <*>? "creation date"
             <*>? "encoding"
             <*>? "info"
             <*>? "info"
