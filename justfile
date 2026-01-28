default:
    just --list

alias fmt := format
format:
    fd -e cabal -x cabal-gild --io
    fd -e nix -X alejandra
    fd -e hs -X fourmolu -m inplace

alias b := build
build:
    cabal build

# https://github.com/haskell/cabal/issues/8544#issuecomment-2564556369
repl:
    cabal repl migadu migaman --enable-multi-repl -b pretty-simple
