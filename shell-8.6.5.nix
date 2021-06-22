import ./shell.nix {
  pkgs = import ./nix/ghc-8.6.5 {};
  compiler = "ghc865";
}
