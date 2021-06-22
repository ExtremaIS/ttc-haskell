import ./shell.nix {
  pkgs = import ./nix/ghc-8.4.4 {};
  compiler = "ghc844";
}
