let

  nixpkgsRev = "cd63096d6d88";
  compilerVersion = "ghc884";

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          haskell-nix = super.callCabal2nix "haskell-nix" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };

  pkgs = import (githubTarball "NixOS" "nixpkgs" nixpkgsRev) { inherit config; };
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

in {

  inherit pkgs;

  shell = compilerSet.shellFor {
    packages = p: [p.haskell-nix];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
    ];
  };

}
