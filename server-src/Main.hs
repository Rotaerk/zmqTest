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
main = void $ concurrently (runZMQEffect eventPublisher) (runZMQEffect commandPuller)

commandPuller :: Z.Context -> Effect (SafeT IO) ()
commandPuller context =
  for (setupProducer context Z.Pull (`Z.bind` "ipc:///tmp/zmqTest-commands")) $
  \message -> liftIO $
    case decode <$> message of
      [Right (c :: IrcCommand)] -> putStrLn $ "Received command: " ++ show c
      [Left er] -> putStrLn $ "Received an invalid command message: " ++ er
      _ -> putStrLn "Unexpectedly received a multi-part message."

eventPublisher :: Z.Context -> Effect (SafeT IO) ()
eventPublisher context =
  setupConsumer context Z.Pub (`Z.bind` "ipc:///tmp/zmqTest-events") ~< do
    liftIO $ threadDelay 2300000
    return $ encode (ReceivedMessage "Who is this?!") :| []
