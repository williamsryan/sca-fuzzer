#!/usr/bin/env bash

set -o errexit -o pipefail -o noclobber -o nounset

# Script to run Revizor n-many times for testing
# the accuracy of our synthesizer.

COUNT=5

for i in $(seq $COUNT); do
    python3 ../src/cli.py fuzz -s ../src/x86/isa_spec/base.json -c ../src/tests/test-detection.yaml -i 50 -n 100 -w ../src
done
