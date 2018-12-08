{-# LANGUAGE OverloadedStrings #-}

module HTorrent.Node where


import           Data.Text                (Text (..))
import           HTorrent.IPC
import           HTorrent.Tracker.UDP
import           HTorrent.Types
import           HTorrent.Utils
import           System.Timeout

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.ByteString          (ByteString (..))

import qualified Data.ByteString          as BS
import qualified Data.Map                 as M
import qualified Data.Text                as T

-- | Given a list of announcers, return the stored peers
--
getPeers :: [Text] -> HTMonad (Maybe [(Host, Port)])
getPeers []       = return Nothing
getPeers (a : as) = do
  es <- liftIO $ newSocketFromAnnounce a
  case es of
    Left err -> do
      -- TODO: Smarter Logging ?
      liftIO $ putStrLn $ "Failed to connect to: " ++ (T.unpack a)
      getPeers as
    Right s  -> do
      -- Get ConnectionId from udpConnecting (Bytes 8-16)
      ec <- liftIO $ udpConnecting s
      case ec of
        Left err' -> liftIO (print ec) >> getPeers as
        Right br -> do
          mi <- asks _hteMetaInfo
          ea <- liftIO $ (udpAnnouncing (sliceBS 8 16 br) mi) s
          case ea of
            Left err'' -> liftIO (print ec) >> getPeers as
            Right a' -> case BS.length a' > 20 of
                         True -> let peers = binDecodePeers (sliceBS 20 (BS.length a') a') in return (Just peers)
                         False -> do
                           liftIO $ putStrLn $ "No peers received from: " ++ (T.unpack a)
                           getPeers as


udpTracker :: HTMonad (Maybe [(Host, Port)])
udpTracker = do
  mi <- asks _hteMetaInfo
  let torrentinfo = _miInfo mi
      announce    = _miAnnounce mi
      announcers  = maybe [[announce]] ((++) [[announce]]) (_miAnnounceList mi)
      announcers' = Prelude.head <$> announcers
      pieces      = _tiPieces (_miInfo mi)
      piecesList  = [(sliceBS i (i + 20) pieces, toInteger $ i `quot` 20) | i <- [0,20..(BS.length pieces)]]
      piecesIndex = M.fromList piecesList :: M.Map Hash Integer
  -- Modify Pieces Index Map (A.K.A Dictionary)
  modify (\s -> s { _htsPiecesIndex = piecesIndex })
  -- Get Peers
  getPeers announcers'
