let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };

    haskell-nix-src =
      let snapshot = builtins.fromJSON (builtins.readFile ./iohk-haskell-nix.json);
      in (import nixpkgs {}).fetchgit {
        name = "haskell-lib";
        inherit (snapshot) url rev sha256 fetchSubmodules;
      };

    pkgs = import nixpkgs (import haskell-nix-src {}).nixpkgsArgs;

    pkgSet = pkgs.haskell-nix.mkStackPkgSet {
      stack-pkgs = import ./pkgs.nix;
      pkg-def-extras = [];
      modules = [];
    };

in
  pkgSet.config.hsPkgs
