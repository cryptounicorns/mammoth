{ mkDerivation, aeson, base, http-api-data, http-client, influxdb
, lens, servant, servant-server, servant-swagger
, servant-swagger-ui, stdenv, swagger2, text, time, transformers
, vector, wai, wai-extra, warp
}:
mkDerivation rec {
  pname = "mammoth-unstable-${version}";
  version = "development";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base http-api-data http-client influxdb lens servant
    servant-server servant-swagger servant-swagger-ui swagger2 text
    time transformers vector wai wai-extra warp
  ];
  executableHaskellDepends = [
    aeson base http-api-data http-client influxdb lens servant
    servant-server servant-swagger servant-swagger-ui swagger2 text
    time transformers vector wai wai-extra warp
  ];
  license = stdenv.lib.licenses.mit;
}
