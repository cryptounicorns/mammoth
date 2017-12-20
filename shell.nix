{ pkgs     ? import <nixpkgs> {}
, compiler ? "ghc802"
}:
{
  env = (import ./default.nix {
    inherit pkgs compiler;
  }).env;

  buildInputs = with pkgs; [
    stack
    cabal2nix
  ];
}
