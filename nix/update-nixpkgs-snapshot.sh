#! /usr/bin/env nix-shell
#! nix-shell -i bash
#! nix-shell -p curl git jq nix

# A more general take of this idea is at
# https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/

set -euxo pipefail

jsonFile=$1;

# git diff-index --quiet HEAD -- || (echo "commit or stash changes"; exit 64)
REV=$(curl -L https://nixos.org/channels/nixos-unstable/git-revision)

if [ ! -f $jsonFile ] || [ ! $(jq '.rev' $jsonFile -r ) == $REV ]; then
    SHA=$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/${REV}.tar.gz)
    jq '{owner: "NixOS", repo: "nixpkgs", rev: $rev, sha256: $sha}' <<< '{}' \
    --arg rev $REV --arg sha $SHA \
        > $jsonFile
    git reset # make sure we aren't commiting anything else
    git add $jsonFile
    git commit -m "$(basename $(pwd)): update nixpkgs snapshot"
fi
