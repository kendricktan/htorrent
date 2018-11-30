{-# LANGUAGE ConstraintKinds #-}

module HTorrent.Types where

import           Control.Monad.Except (ExceptT (..))

data Env = Env Int
