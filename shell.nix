{ pkgs     ? import <nixpkgs> {}
, compiler ? "ghc802"
}:
pkgs.stdenv.mkDerivation {
  name = "mammoth-shell";
  buildInputs = with pkgs; [
    influxdb
    stack
    #haskellPackages.ghc-mod
    cabal2nix
  ];
  # ++ (import ./default.nix { inherit pkgs compiler; }).all;
}
