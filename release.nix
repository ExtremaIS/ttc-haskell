{
  ttc-ghc-822 = import ./shell.nix {
    pkgs = import ./nix/ghc-8.4.4 {};
    compiler = "ghc822";
  };
  ttc-ghc-844 = import ./shell.nix {
    pkgs = import ./nix/ghc-8.4.4 {};
    compiler = "ghc844";
  };
  ttc-ghc-865 = import ./shell.nix {
    pkgs = import ./nix/ghc-8.6.5 {};
    compiler = "ghc865";
  };
  ttc-ghc-884 = import ./shell.nix {
    pkgs = import ./nix/ghc-8.10.4 {};
    compiler = "ghc884";
  };
  ttc-ghc-8104 = import ./shell.nix {
    pkgs = import ./nix/ghc-8.10.4 {};
    compiler = "ghc8104";
  };
  ttc-ghc-901 = import ./shell.nix {
    pkgs = import ./nix/ghc-9.0.1 {};
    compiler = "ghc901";
  };
}
