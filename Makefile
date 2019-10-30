PROJECT := crispy-broccoli

build: default.nix
	nix-build shell.nix

shell: default.nix ${PROJECT}.cabal
	nix-shell --command "export PS1='[${PROJECT}] $(value PS1)'; return"

repl: default.nix ${PROJECT}.cabal
	nix-shell --run 'cabal v1-repl ${PROJECT}'

.PHONY: test
test: default.nix ${PROJECT}.cabal
	nix-shell --run 'cabal v1-test --ghc-option=-O0'

default.nix: package.yaml
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'

${PROJECT}.cabal: package.yaml
	find . -name '*_flymake.hs' -delete
ifeq ("${IN_NIX_SHELL}",)
	hpack
else
	nix-shell -p haskellPackages.hpack --run 'hpack'
endif
