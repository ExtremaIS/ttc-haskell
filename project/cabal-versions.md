# Cabal Versions

GHC requirements:

* [GHC 8.2.2](https://www.haskell.org/ghc/download_ghc_8_2_2.html) Cabal 1.24
* [GHC 8.4.4](https://www.haskell.org/ghc/download_ghc_8_4_4.html) Cabal 2.2
* [GHC 8.6.5](https://www.haskell.org/ghc/download_ghc_8_6_5.html) Cabal 2.4
* [GHC 8.8.4](https://www.haskell.org/ghc/download_ghc_8_8_4.html) Cabal 3.0
* [GHC 8.10.4](https://www.haskell.org/ghc/download_ghc_8_10_4.html) Cabal 3.2
* [GHC 9.0.1](https://www.haskell.org/ghc/download_ghc_9_0_1.html) Cabal 3.4
* [GHC 9.2.1](https://www.haskell.org/ghc/download_ghc_9_2_1.html) Cabal 3.6

[GibHub Actions Versions](https://github.com/haskell/actions/blob/main/setup/src/versions.json):

* Cabal 2.2.0.0 install fails

```
Installing cabal version 2.2.0.0
  Attempting to install cabal-install 2.2 using apt-get
  /usr/bin/sudo -- sh -c apt-get -y install cabal-install-2.2
  Reading package lists...
  Building dependency tree...
  Reading state information...
  E: Unable to locate package cabal-install-2.2
  E: Couldn't find any package by glob 'cabal-install-2.2'
  E: Couldn't find any package by regex 'cabal-install-2.2'
  Attempting to install cabal 2.2.0.0 using ghcup
  /opt/hostedtoolcache/ghcup/0.1.12/x64/ghcup install cabal 2.2.0.0
  [ Warn  ] New GHCup version available: 0.1.14. To upgrade, run 'ghcup upgrade'
  [ Error ] No available Cabal version for 2.2.0.0
Error: All install methods for cabal 2.2.0.0 failed
```
