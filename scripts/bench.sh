#! /usr/bin/env bash

# Usage: bench.sh
#
# Runs random:bench and outputs the results in a CSV file with the name
# "<git branch>-<git commit>.csv".

set -euo pipefail

OUTPUT="$(git branch --show-current)-$(git rev-parse --short HEAD).csv"
stack bench random:bench --ba "--csv=${OUTPUT} --small"
echo "Results are in $OUTPUT"
