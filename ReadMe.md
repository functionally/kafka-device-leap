Leap Motion events via a Kafka message broker
=============================================


This package contains functions for passing [Leap Motion](https://www.leapmotion.com/product/desktop) events to topics on a [Kafka message broker](https://kafka.apache.org/).


Clients
-------

The simple Kafka client that produces events from Leap Motion can be run, for example, as follows:

	cabal run kafka-device-leap -- localhost 6437 leap-client localhost 9092 events leap

Also see https://hackage.haskell.org/package/kafka-device/.
