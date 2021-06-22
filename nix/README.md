# Nix Configuration

This Nix configuration uses [nix-thunk][] to specify [nixpkgs][] revisions for
different GHC versions.

[nix-thunk]: <https://github.com/obsidiansystems/nix-thunk#nix-thunk>
[nixpkgs]: <https://github.com/NixOS/nixpkgs>

## Software

[Install](https://github.com/obsidiansystems/nix-thunk#installation) the
`nix-thunk` command as follows:

```
$ nix-env \
    -f https://github.com/obsidiansystems/nix-thunk/archive/master.tar.gz \
    -iA command
```

## Configuration

From the project directory, configure a new revision as follows:

```
$ nix-thunk create \
    https://github.com/NixOS/nixpkgs.git \
    --rev c92ca95afb5043bc6faa0d526460584eccff2277 \
    nix/ghc-8.10.4
```

Revisions reference:

<https://lazamar.co.uk/nix-versions/?channel=nixpkgs-unstable&package=ghc>

## Usage

### Shell

Shell configuration can be found in `shell*.nix` in the project directory.
Run a shell for a specific GHC version as follows:

```
$ nix-shell shell-8.10.4.nix
```

### Testing

TTC can be tested against all of the supported GHC versions using as follows:

```
$ nix-build release.nix
```
