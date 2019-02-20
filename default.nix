{
  mkDerivation, stdenv
, aeson, base, hleap, kafka-device, websockets
}:

mkDerivation {
  pname = "kafka-device-leap";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base hleap kafka-device websockets
  ];
  executableHaskellDepends = [
  ];
  homepage = "https://bitbucket.org/functionally/kafka-device-leap";
  description = "Leap Motion events via a Kafka message broker";
  license = stdenv.lib.licenses.mit;
}
