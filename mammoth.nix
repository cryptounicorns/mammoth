{ mkDerivation, aeson, base, envy, http-api-data, http-client
, influxdb, lens, servant, servant-server, servant-swagger
, servant-swagger-ui, stdenv, strings, swagger2, text, time, transformers
, vector, wai, wai-extra, warp
}:
mkDerivation rec {
  pname = "mammoth-unstable-${version}";
  version = "development";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base envy http-api-data http-client influxdb lens servant
    servant-server servant-swagger servant-swagger-ui strings swagger2 text
    time transformers vector wai wai-extra warp
  ];
  executableHaskellDepends = [
    aeson base envy http-api-data http-client influxdb lens servant
    servant-server servant-swagger servant-swagger-ui strings swagger2 text
    time transformers vector wai wai-extra warp
  ];
  license = stdenv.lib.licenses.mit;
}
