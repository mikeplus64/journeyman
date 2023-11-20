{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import Tourney.Algebra
import Tourney.UI.Main qualified

knownTournaments :: [(Text, Tournament TMany)]
knownTournaments = Tourney.UI.Main.defaultTournaments

main :: IO ()
main = () <$ Tourney.UI.Main.createTourneyUI knownTournaments
