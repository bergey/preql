let
  inherit (import ./nix/versions.nix) project haskellPackages nixpkgs;
in
    nixpkgs.mkShell {
        inputsFrom = [ project ];
        buildInputs = [
            haskellPackages.cabal-install
            nixpkgs.ghcid
            nixpkgs.stylish-haskell
            ];
        shellHook = ''export 'PS1=[crispy-broccoli] ${builtins.getEnv "PS1"}' '';
    }
