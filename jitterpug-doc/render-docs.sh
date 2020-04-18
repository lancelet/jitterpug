#!/usr/bin/env bash
#
# Generate documentation.

# Install tools if necessary
if ! stack exec which "inlitpp" > /dev/null
then
    stack build --copy-compiler-tool "inlitpp"
fi

# Run the documentation generator
stack exec jitterpug-doc
