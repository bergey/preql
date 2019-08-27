{ mkDerivation, attoparsec, base, bytestring, contravariant, free
, hpack, postgresql-libpq, postgresql-simple, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, vector
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring contravariant free postgresql-libpq
    postgresql-simple text transformers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    attoparsec base bytestring contravariant free postgresql-libpq
    postgresql-simple tasty tasty-hunit tasty-quickcheck text
    transformers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/bergey/crispy-broccoli#readme";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
