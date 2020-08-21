#!/usr/bin/env bash

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

yarn lint "$@"
