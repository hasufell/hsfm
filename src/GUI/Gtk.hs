{-# OPTIONS_HADDOCK ignore-exports #-}

module Main where

import Graphics.UI.Gtk
import GUI.Gtk.Gui

main :: IO ()
main = do
  _ <- initGUI

  startMainWindow

  mainGUI
