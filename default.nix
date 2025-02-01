{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "quantizer";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    base minmax monoid-insertleft uniqueness-periods-vector-stats
  ];
  homepage = "https://hackage.haskell.org/package/quantizer";
  description = "Library to provide the behaviour similar to quantum states superposition";
  license = pkgs.lib.licenses.mit;
}
