# Journeyman

An eDSL for creating tournaments. Use it as a library in a project that uses
tournaments.

# Documentation access

Documentation is hosted at https://mikeplus64.github.io/journeyman

# Development setup

You can set up a development environment with journeyman by using
[Nix](https://nixos.org/).

``` sh
$ git clone git@github.com:mikeplus64/journeyman.git
$ cd journeyman
$ nix develop
$ just repl
  # Much loading and module compilation goes here :-)
$ :l Tourney.UI.Main
$ :main
```

I have tested journeyman on Linux only, although macOS should work easily enough
as well. On Windows, it may be easier to set up a Haskell environment through
Windows Subsystem for Linux.
