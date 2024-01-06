#!/usr/bin/env bash
#
# Run the test, called in TestGeneration.hs

set -eux -o pipefail

builtin cd "$(dirname "${BASH_SOURCE}")"

LOG_FILE=$1
exec &> "${LOG_FILE}"

graphql-codegen

cat "${GHC_ENVIRONMENT}"

ghc \
    -v \
    --make \
    -odir gen/out/ \
    -hidir gen/out/ \
    -isrc:gen \
    Example.Scalars \
    Example.Enums.Gender \
    Example.API
