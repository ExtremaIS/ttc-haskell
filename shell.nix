{ pkgs ? import ./nix/default {}
, compiler ? "ghc8103"
}:
let
  gitIgnore = (import ./nix/default {}).nix-gitignore.gitignoreSourcePure;
in
  pkgs.haskell.packages.${compiler}.developPackage {
    root = gitIgnore [./.gitignore] ./.;
    name = "ttc";
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
        ]);
  }
