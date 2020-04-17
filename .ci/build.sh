#!/usr/bin/env bash
#
# CI build script
stack --no-terminal test --haddock --no-haddock-deps
pushd jitterpug-doc > /dev/null && \
      ./render-docs.sh          && \
      popd > /dev/null
