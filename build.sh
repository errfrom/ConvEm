#!/bin/sh

stack clean
stack build --executable-stripping --library-stripping 
cp ./.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin/DDChat-exe ./Sheer-0.1.0.0-linux-x86_64/Sheer
stack clean
