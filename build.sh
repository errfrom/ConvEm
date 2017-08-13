#!/bin/sh
CABAL_ALL=`cat DDChat.cabal`
VERSION_STRING=$(echo "$CABAL_ALL" | grep "^version:.*$")
VERSION=$(echo "$VERSION_STRING" | sed 's/version: *//')
SHEER_DIR="Sheer-linix-$VERSION"
mkdir "$SHEER_DIR"
stack build
cp ./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/DDChat-exe/DDChat-exe "$SHEER_DIR/Sheer"
stack clean
cp -R ./static "$SHEER_DIR"
