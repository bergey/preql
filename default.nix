{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/bergey/crispy-broccoli";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
