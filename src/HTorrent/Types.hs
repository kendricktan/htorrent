{-# LANGUAGE ConstraintKinds #-}

module HTorrent.Types where

import           Control.Monad.Except (ExceptT (..))
import           Data.ByteString      (ByteString (..))
import           Data.Either
import           Data.Map.Strict      (Map (..))
import           Data.Maybe           (Maybe (..))

import qualified Data.ByteString      as B
import qualified Data.Map.Strict      as M


data SingleFileMode = SingleFileMode
  { _sfName   :: String
  , _sfLength :: Integer
  , _sfMd5Sum :: Maybe String
  }

data MultiFileModeFiles = MultiFileModeFiles
  { _mfmName   :: String
  , _mfmMd5Sum :: Maybe String
  , _mfmPath   :: String
  }

data MultiFileMode = MultiFileMode
  { _mfName  :: String
  , _mfFiles :: [MultiFileModeFiles]
  }

data TorrentInfo = TorrentInfo
  { _tiPieceLength :: Integer
  , _tiPieces      :: [ByteString]
  , _tiPrivate     :: Maybe Int
  , _tiInfo        :: Either SingleFileMode MultiFileMode
  }

data TorrentFile = TorrentFile
  { _info :: Int
  }
