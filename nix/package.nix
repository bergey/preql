{ mkDerivation, alex, array, attoparsec, base, bytestring
, contravariant, free, generic-random, happy, hpack
, postgresql-libpq, postgresql-simple, QuickCheck, stdenv, syb
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-lift-instances, transformers, vector
, nix-gitignore
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1";
  src = nix-gitignore.gitignoreSource [] ./..;
  libraryHaskellDepends = [
    array attoparsec base bytestring contravariant free
    postgresql-libpq postgresql-simple syb template-haskell text
    th-lift-instances transformers vector
  ];
  libraryToolDepends = [ alex happy hpack ];
  testHaskellDepends = [
    array attoparsec base bytestring contravariant free generic-random
    postgresql-libpq postgresql-simple QuickCheck syb tasty tasty-hunit
    tasty-quickcheck template-haskell text th-lift-instances
    transformers vector
  ];
  testToolDepends = [ alex happy ];
  prePatch = "hpack";
  homepage = "https://github.com/bergey/crispy-broccoli#readme";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
