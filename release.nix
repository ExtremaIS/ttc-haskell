# Add new nixpkgs thunk by using nix-thunk
# > nix-env -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz -iA command
# Example:
# > nix-thunk create https://github.com/NixOS/nixpkgs.git --rev de7e0f7dee2deeda620e0c940362e2e63ad5f6bf nix/ghc8901

{
  ttc-ghc-822 = import ./shell.nix {
    pkgs = import ./nix/ghc844 {};
    compiler = "ghc822";
  };
  ttc-ghc-844 = import ./shell.nix {
    pkgs = import ./nix/ghc844 {};
    compiler = "ghc844";
  };
  ttc-ghc-865 = import ./shell.nix {
    pkgs = import ./nix/ghc865 {};
    compiler = "ghc865";
  };
  ttc-ghc-884 = import ./shell.nix {
    pkgs = import ./nix/ghc901 {};
    compiler = "ghc884";
  };
  ttc-ghc-8104 = import ./shell.nix {
    pkgs = import ./nix/ghc901 {};
    compiler = "ghc8104";
  };
  ttc-ghc-901 = import ./shell.nix {
    pkgs = import ./nix/ghc901 {};
    compiler = "ghc901";
  };
}
