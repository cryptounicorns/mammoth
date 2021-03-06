with import <nixpkgs>{};
{ pkgs ? import <nixpkgs> {} }:

buildGo19Package rec {
  name = "mammoth-unstable-${version}";
  version = "development";

  buildInputs = with pkgs; [ git glide ];

  checkTarget = "test";

  src = ./.;
  goPackagePath = "github.com/cryptounicorns/mammoth";
}
