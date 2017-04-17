{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Serialize
import System.ZMQ4.Monadic

main :: IO ()
main = void $ concurrently eventSubscriber commandPusher

commandPusher :: IO ()
commandPusher = runZMQ $ do
  pusher <- socket Push
  connect pusher "ipc://zmqTest-commands"

  forever $ do
    liftIO $ threadDelay 10000000
    send pusher [] $ encode (SendMessage "Hi there!")

eventSubscriber :: IO ()
eventSubscriber = runZMQ $ do
  subscriber <- socket Sub
  connect subscriber "ipc://zmqTest-events"
  subscribe subscriber ""

  forever $ do
    codedMessage <- receive subscriber
    liftIO $ case decode codedMessage of
      Left er -> putStrLn $ "Received an invalid event message: " ++ er
      Right (ev :: IrcEvent) -> putStrLn $ "Received event: " ++ show ev
