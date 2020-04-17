#!/usr/bin/env bash
#
# Generate documentation.

# Install tools if necessary
if ! stack exec which "tintin" > /dev/null
then
    stack build --copy-compiler-tool "tintin"
fi
if ! stack exec which "inlitpp" > /dev/null
then
    stack build --copy-compiler-tool "inlitpp"
fi

# Run tintin
stack exec tintin -- run --verbose

# Hint at location
echo "tintin files output to $(pwd)/.stack-work/tintin/rendered/index.html"
