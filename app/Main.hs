{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Tourney.Algebra
import Tourney.Prelude
import Tourney.UI.Main qualified

knownTournaments :: [(Text, Tournament TMany)]
knownTournaments = Tourney.UI.Main.defaultTournaments

main :: IO ()
main = () <$ Tourney.UI.Main.createTourneyUI knownTournaments
