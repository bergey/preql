PACKAGE=preql

shell:
	nix-shell nix -A ${PACKAGE}.components.all.profiled
