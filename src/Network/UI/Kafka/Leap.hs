{-|
Module      :  Network.UI.Kafka.Leap
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Stable

Producer events from a Leap Motion \<<https://www.leapmotion.com/product/desktop>\> controller.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.Leap (
-- * Event handling
  leapApp
) where


import Control.Monad (void)
import Data.Aeson (eitherDecode)
import Network.Kafka (KafkaAddress, KafkaClientId)
import Network.Kafka.Protocol (TopicName)
import Network.UI.Kafka (Sensor, producerLoop)
import Network.UI.Kafka.Types as K (Event(..), Finger(..), Hand(..))
import Network.WebSockets (receiveData)
import System.Hardware.Leap (ClientApp, setFocused, setGestures)
import System.Hardware.Leap.Event as L (Event(..))
import System.Hardware.Leap.Event.Hand as L (Hand(..), Side(..))
import System.Hardware.Leap.Event.Pointable as L (Finger(..), Pointable(..))


-- | WebSocket application for producing Leap Motion events.
leapApp :: KafkaClientId -- ^ A Kafka client identifier for the producer.
        -> KafkaAddress  -- ^ The address of the Kafka broker.
        -> TopicName     -- ^ The Kafka topic name.
        -> Sensor        -- ^ The name of the sensor producing events.
        -> ClientApp ()  -- ^ The WebSocket client application.
leapApp clientId address topic sensor connection =
  do
    setFocused  True  connection
    setGestures False connection
    (_, loop) <-
      producerLoop clientId address topic sensor
        $ do
            event <- eitherDecode <$> receiveData connection
            case event of
              Right e -> return $ interpret e
              Left  s -> return [EventError s]
    void loop


-- | Interpret a Leap Motion event as events for Kafka.
interpret :: L.Event Double -> [K.Event]
interpret Tracking{..} = concatMap interpretPointable pointables
interpret _            = [EventError "Unable to process event."]


-- | Interpret a Leap Motion pointing event as events for Kafka.
interpretPointable :: Pointable Double -> [K.Event]
interpretPointable Finger{..} =
  [
    FingerEvent
    {
      hand            = case side hand of
                          L.LeftHand     -> K.LeftHand
                          L.RightHand    -> K.RightHand
    , finger          = case finger of
                          L.Thumb        -> K.Thumb
                          L.IndexFinger  -> K.IndexFinger
                          L.MiddleFinger -> K.MiddleFinger
                          L.RingFinger   -> K.RingFinger
                          L.Pinky        -> K.Pinky
    , pointerPosition = stabilizedTipPosition
    }
  ]
interpretPointable Tool{..}   =
  [
    PointerEvent
    {
      pointerPosition = stabilizedTipPosition
    }
  ]
interpretPointable _          = []
