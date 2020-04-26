#!/usr/bin/env bash
#
# Format the source code.

# Install tools if necessary
declare -ra tools=('cabal-fmt' 'ormolu')
for tool_name in "${tools[@]}"
do
    if ! stack exec which "$tool_name"
    then
        stack build --copy-compiler-tool "$tool_name"
    fi
done

# Run tools (in the correct order)
stack exec cabal-fmt -- --tabular -i jitterpug.cabal
stack exec ormolu -- --mode=inplace **/*.hs
