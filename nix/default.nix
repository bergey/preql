let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs {
    overlays = [
      (_: _: { niv = import sources.niv {}; })
    ];
  };
in with nixpkgs;
mkShell {
  name = "preql";
  buildInputs = [
    cabal-install
    haskell.compiler.ghc8102
    haskellPackages.alex
    haskellPackages.happy
    haskellPackages.hpack

    postgresql_12
    zlib
  ];
}
