{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc802"
}:
{
  env = (import ./default.nix {
    inherit nixpkgs compiler;
  }).env;

  buildInputs = with nixpkgs.pkgs; [
    stack
    cabal2nix
  ];
}
