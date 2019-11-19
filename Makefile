PROJECT := crispy-broccoli

build: nix/package.nix
	nix-build nix/versions.nix -A project

shell: nix/package.nix ${PROJECT}.cabal
	nix-shell

repl: nix/package.nix ${PROJECT}.cabal
# TODO check for direnv / run without nix-shell
	nix-shell --run 'cabal v1-repl ${PROJECT}'

.PHONY: test
test: nix/package.nix ${PROJECT}.cabal
	nix-shell --run 'cabal v1-test --ghc-option=-O0'

nix/package.nix: package.yaml
	nix-shell -p cabal2nix --run 'cabal2nix . > nix/package.nix'

${PROJECT}.cabal: package.yaml
	find . -name '*_flymake.hs' -delete
ifeq ("${IN_NIX_SHELL}",)
	hpack
else
	nix-shell -p haskellPackages.hpack --run 'hpack'
endif
