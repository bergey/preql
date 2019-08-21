{ mkDerivation, base, postgresql-libpq, postgresql-simple, stdenv
, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "crispy-broccoli";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base postgresql-libpq postgresql-simple text
  ];
  testHaskellDepends = [
    base postgresql-libpq tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "https://github.com/bergey/crispy-broccoli";
  description = "experiments with SQL";
  license = stdenv.lib.licenses.bsd3;
}
