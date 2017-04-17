{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Lib (
  IrcCommand (..),
  IrcEvent (..)
) where

import Data.ByteString as BS
import Data.Serialize
import GHC.Generics

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
