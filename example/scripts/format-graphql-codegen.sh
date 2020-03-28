#!/usr/bin/env bash

set -eux -o pipefail

# root directory of this project; i.e. the directory containing
# graphql-client-example.cabal
ROOT="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

FILES=()
for FILE in "$@"; do
    FILES+=("${ROOT}/${FILE}")
done

exec "${ROOT}/../scripts/stylish-haskell.sh" --apply "${FILES[@]}"
