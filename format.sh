#!/usr/bin/env bash
#
# Format source code.
set -e
pushd jitterpug > /dev/null && \
    ./format.sh &&             \
    popd > /dev/null
pushd jitterpug-benchmarks > /dev/null && \
    ./format.sh &&                        \
    popd > /dev/null
pushd jitterpug-doc > /dev/null && \
    ./format.sh &&                 \
    popd > /dev/null
