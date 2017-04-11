{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
  responder <- socket Rep
  bind responder "tcp://*:5555"

  forever $ do
    buffer <- receive responder
    liftIO $ do
      putStrLn "Received Hello"
      threadDelay 1000000
    send responder [] "World"
