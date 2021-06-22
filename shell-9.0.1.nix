import ./shell.nix {
  pkgs = import ./nix/ghc-9.0.1 {};
  compiler = "ghc901";
}
