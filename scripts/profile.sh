#!/bin/sh

stack clean
stack build --executable-stripping --library-stripping --profile
stack exec -- Glob-exe +RTS -sstderr
