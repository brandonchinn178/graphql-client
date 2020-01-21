#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

ARGS=()

for ARG in "$@"; do
    ARGS+=("${ARG#graphql-codegen-haskell/}")
done

yarn graphql run lint "${ARGS[@]:-.}"
