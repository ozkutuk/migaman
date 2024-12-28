default:
    just --list

# https://github.com/haskell/cabal/issues/8544#issuecomment-2564556369
repl:
    cabal repl all --enable-multi-repl -b pretty-simple

alias b := build
build:
    cabal build
