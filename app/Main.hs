{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import Tourney.UI.Main qualified

main :: IO ()
main = () <$ Tourney.UI.Main.main
