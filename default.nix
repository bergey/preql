{ mkDerivation, alex, array, attoparsec, base, bytestring
, contravariant, free, generic-random, happy, hpack
, postgresql-libpq, postgresql-simple, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, vector
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    array attoparsec base bytestring contravariant free
    postgresql-libpq postgresql-simple text transformers vector
  ];
  libraryToolDepends = [ alex happy hpack ];
  testHaskellDepends = [
    array attoparsec base bytestring contravariant free generic-random
    postgresql-libpq postgresql-simple QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  testToolDepends = [ alex happy ];
  prePatch = "hpack";
  homepage = "https://github.com/bergey/crispy-broccoli#readme";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
