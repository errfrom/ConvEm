#!/bin/sh

cd ..
stack clean
stack build --profile --ghc-options="-static "
stack exec -- Glob-exe +RTS -p -h -sstderr
cd scripts
mkdir prof-results
mv ../Glob-exe.hp prof-results/
mv ../Glob-exe.prof prof-results/
