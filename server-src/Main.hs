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
main = void $ concurrently eventPublisher commandPuller

commandPuller :: IO ()
commandPuller = runZMQ $ do
  puller <- socket Pull
  bind puller "ipc://zmqTest-commands"

  forever $ do
    codedMessage <- receive puller
    liftIO $ case decode codedMessage of
      Left er -> putStrLn $ "Received an invalid command message: " ++ er
      Right (c :: IrcCommand) -> putStrLn $ "Received command: " ++ show c

eventPublisher :: IO ()
eventPublisher = runZMQ $ do
  publisher <- socket Pub
  bind publisher "ipc://zmqTest-events"

  forever $ do
    liftIO $ threadDelay 2300000
    send publisher [] $ encode (ReceivedMessage "Who is this?!")
