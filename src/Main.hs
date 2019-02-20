{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Production
Portability :  Linux

Simple producer of events on a Kafka topic from a Leap Motion \<<https://www.leapmotion.com/product/desktop>\> controller.
-}


module Main (
-- * Main entry
  main
) where


import Network.UI.Kafka (TopicConnection(TopicConnection))
import Network.UI.Kafka.Leap (leapApp)
import System.Environment (getArgs)
import System.Hardware.Leap (Configuration(..), run)


-- | The main action.
main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [leapHost, leapPort, client, kafkaHost, kafkaPort, topic, sensor] ->
        do
          putStrLn $ "Leap address:  (" ++ leapHost ++ "," ++ leapPort ++ ")"
          putStrLn $ "Kafka client:  " ++ client
          putStrLn $ "Kafka address: (" ++ kafkaHost ++ "," ++ kafkaPort ++ ")"
          putStrLn $ "Kafka topic:   " ++ topic
          putStrLn $ "Sensor name:   " ++ sensor
          run (Configuration leapHost $ read leapPort)
            $ leapApp
              (TopicConnection client (kafkaHost, read kafkaPort) topic)
              sensor
      _ -> putStrLn "USAGE: kafka-device-leap leap-host leap-port client kafka-host kafka-port topic senosr"
