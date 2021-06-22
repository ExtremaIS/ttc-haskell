import ./shell.nix {
  pkgs = import ./nix/ghc-8.10.4 {};
  compiler = "ghc8104";
}
