{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.List.NonEmpty
import Data.Serialize
import Pipes.ZMQ4
import qualified System.ZMQ4 as Z

main :: IO ()
main = void $ concurrently (runZMQEffect eventSubscriber) (runZMQEffect commandPusher)

commandPusher :: Z.Context -> Effect (SafeT IO) ()
commandPusher context =
  setupConsumer context Z.Push (`Z.connect` "ipc:///tmp/zmqTest-commands") ~< do
    liftIO $ threadDelay 10000000
    return $ encode (SendMessage "Hi there!") :| []

eventSubscriber :: Z.Context -> Effect (SafeT IO) ()
eventSubscriber context =
  for (
    setupProducer context Z.Sub $
    \sub -> do
      Z.connect sub "ipc:///tmp/zmqTest-events"
      Z.subscribe sub ""
  ) $
  \message -> liftIO $
    case decode <$> message of
      [Right (ev :: IrcEvent)] -> putStrLn $ "Received event: " ++ show ev
      [Left er] -> putStrLn $ "Received an invalid event message: " ++ er
      _ -> putStrLn "Unexpectedly received a multi-part message."
