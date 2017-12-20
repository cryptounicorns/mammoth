{ mkDerivation, aeson, base, http-api-data, http-client, influxdb
, lens, servant, servant-docs, servant-server, stdenv, text, time
, transformers, vector, wai, wai-extra, warp
}:
mkDerivation rec {
  pname = "mammoth-unstable-${version}";
  version = "development";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base http-api-data http-client influxdb lens servant
    servant-docs servant-server text time transformers vector wai
    wai-extra warp
  ];
  executableHaskellDepends = [
    aeson base http-api-data http-client influxdb lens servant
    servant-docs servant-server text time transformers vector wai
    wai-extra warp
  ];
  license = stdenv.lib.licenses.mit;
}
