# Nix configuration for testing TTC against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  ttc-ghc-822 = import ./shell.nix { compiler = "ghc822"; };
  ttc-ghc-844 = import ./shell.nix { compiler = "ghc844"; };
  ttc-ghc-865 = import ./shell.nix { compiler = "ghc865"; };
  ttc-ghc-884 = import ./shell.nix { compiler = "ghc884"; };
  ttc-ghc-8104 = import ./shell.nix { compiler = "ghc8104"; };
  ttc-ghc-901 = import ./shell.nix { compiler = "ghc901"; };
}
