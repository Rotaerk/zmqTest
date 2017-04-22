{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Lib (
  IrcCommand (..),
  IrcEvent (..),
  runZMQEffect
) where

import Data.ByteString as BS
import Data.Serialize
import GHC.Generics
import Pipes.ZMQ4
import qualified System.ZMQ4 as Z

data IrcCommand =
  SendMessage BS.ByteString |
  Disconnect BS.ByteString |
  Connect BS.ByteString Int
  deriving (Show, Generic)

instance Serialize IrcCommand

data IrcEvent =
  Connected |
  Disconnected |
  ReceivedMessage BS.ByteString
  deriving (Show, Generic)

instance Serialize IrcEvent

runZMQEffect :: (Z.Context -> Effect (SafeT IO) a) -> IO a
runZMQEffect e = Z.withContext $ runSafeT . runEffect . e
