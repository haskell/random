#!/usr/bin/env bash

set -euo pipefail

cabal repl --build-depends=unliftio --with-compiler=doctest --repl-options='-w -Wdefault'
