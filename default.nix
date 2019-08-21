{ mkDerivation, attoparsec, base, bytestring, contravariant, free
, postgresql-libpq, postgresql-simple, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, transformers, vector
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring contravariant free postgresql-libpq
    postgresql-simple text transformers vector
  ];
  testHaskellDepends = [
    base postgresql-libpq tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/bergey/crispy-broccoli";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
