{ mkDerivation, alex, array, attoparsec, base, bytestring
, contravariant, free, hpack, postgresql-libpq, postgresql-simple
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, transformers
, vector
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    array attoparsec base bytestring contravariant free
    postgresql-libpq postgresql-simple text transformers vector
  ];
  libraryToolDepends = [ alex hpack ];
  testHaskellDepends = [
    array attoparsec base bytestring contravariant free
    postgresql-libpq postgresql-simple tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  testToolDepends = [ alex ];
  prePatch = "hpack";
  homepage = "https://github.com/bergey/crispy-broccoli#readme";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
