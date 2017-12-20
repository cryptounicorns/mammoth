with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "mamoth-shell";
  buildInputs = [
    stack
  ];
}
