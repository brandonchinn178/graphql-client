#!/usr/bin/env bash

set -eux -o pipefail

# root directory of this project; i.e. the directory containing
# graphql-client-example.cabal
ROOT="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

stack exec -- graphql-codegen -c "${ROOT}/codegen.yml"
