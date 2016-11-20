module Main (
  main
) where


import Data.String (IsString(fromString))
import Network.UI.Kafka.Leap (leapApp)
import System.Environment (getArgs)
import System.Hardware.Leap (Configuration(..), run)


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
              (fromString client)
              (fromString kafkaHost, toEnum $ read kafkaPort)
              (fromString topic)
              sensor
      _ -> putStrLn "USAGE: kafka-device-keyboard leap-host leap-port client kafka-host kafka-port topic senosr"
