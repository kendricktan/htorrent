{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:

let
  # Can't use overlays as there is an infinite
  # recursion in the list of dependencies that needs
  # to be fixed first
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  htorrentDeps = import ./htorrent-deps.nix;

  modifiedHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (htorrentDeps pkgs);
  });

  htorrent = modifiedHaskellPackages.callPackage ./htorrent.nix {};

in
  htorrent
