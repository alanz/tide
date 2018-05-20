let
  localLib = import ./nix/lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

with pkgs;

let
  happy_private = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callPackage ./nix/happy.nix { });

in
stdenv.mkDerivation {
  name = "azplay";

  buildInputs = [
    nix bash binutils coreutils curl gnutar
    gnumake
    happy_private
    haskellPackages.alex
    cabal-install
    ghc
  ];
}
