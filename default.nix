{ mkDerivation, base, mod-n, stdenv, vector }:
mkDerivation {
  pname = "n-vector";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mod-n vector ];
  license = stdenv.lib.licenses.mit;
}
