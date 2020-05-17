{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./ttc.nix { }
