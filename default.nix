{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};
  htorrent = haskellPackages.callPackage ./htorrent.nix {};

in
  htorrent
