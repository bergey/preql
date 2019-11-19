let
    snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
    inherit (snapshot) owner repo rev;
    tarball = builtins.fetchTarball {
        inherit (snapshot) sha256;
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };
    nixpkgs = import tarball {};
    haskellPackages =nixpkgs.haskellPackages;
    project = import ./package.nix;

in {
  project = haskellPackages.callPackage project {};
  inherit nixpkgs haskellPackages;
}

