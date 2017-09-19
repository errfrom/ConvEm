#! /bin/bash

function create_app_dir()
{
  CABAL_ALL=`cat ../Glob.cabal`
  VERSION_STRING=$(echo "$CABAL_ALL" | grep "^version:.*$")
  VERSION=$(echo "$VERSION_STRING" | sed 's/version: *//')
  DIR_NAME="Glob-$VERSION-linix-x86_64"
  mkdir "$DIR_NAME"
}

stack clean
create_app_dir
stack build --executable-stripping --library-stripping
cp ../.stack-work/install/x86_64-linux/lts-*/*/bin/*-exe "$DIR_NAME/Glob"
stack clean
