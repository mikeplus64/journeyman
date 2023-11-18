default:
  @just --list

# Run hoogle
docs:
  echo http://127.0.0.1:8888
  hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
  cabal repl {{ARGS}}

# Run cabal
cabal *ARGS:
  cabal {{ARGS}}

# Autoformat the project tree
fmt:
  treefmt

# Run ghcid -- auto-recompile and run `main` function
run:
  ghcid -c "cabal repl exe:journeyman" --warnings -T :main

# Run ghcid -- auto-recompile and run `main` function
test:
  ghcid -c "cabal repl test:tests" --warnings --colour -T ":main --color=always"

