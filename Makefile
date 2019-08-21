build: default.nix
	nix-build shell.nix

default.nix: crispy-broccoli.cabal
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'

shell: default.nix
	nix-shell

.PHONY: test
test: default.nix
	nix-shell --run 'cabal v1-test'

repl: default.nix
	nix-shell --run 'cabal v1-repl crispy-broccoli'
