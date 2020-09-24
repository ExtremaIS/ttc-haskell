{ mkDerivation, stdenv }:
mkDerivation {
  pname = "ttc-examples";
  version = "0.2.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  homepage = "https://github.com/ExtremaIS/ttc-haskell#readme";
  description = "Textual Type Classes Examples";
  license = stdenv.lib.licenses.mit;
}
