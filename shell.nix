{ pkgs     ? import <nixpkgs> {}
, compiler ? "ghc802"
}:
pkgs.stdenv.mkDerivation {
  name = "mammoth-shell";
  buildInputs = let
    tools = with pkgs; [
      influxdb
      stack
      cabal2nix
    ];
    haskellTools = with pkgs.haskellPackages; [
      stylish-haskell
      #ghc-mod
    ];
    app = (import ./default.nix { inherit pkgs compiler; }).all;
  in tools ++ haskellTools;
}
