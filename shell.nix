with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "nix-cage-shell";
  buildInputs = [
    influxdb
    go
    gocode
    glide
    godef
    lua
  ];
  shellHook = ''
    export GOPATH=~/projects
  '';
}
