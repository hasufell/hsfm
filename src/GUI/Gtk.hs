{-# OPTIONS_HADDOCK ignore-exports #-}

module Main where

import Graphics.UI.Gtk
import GUI.Gtk.Gui
import Safe
  (
    headDef
  )
import System.Environment
  (
    getArgs
  )


main :: IO ()
main = do
  _ <- initGUI

  args <- getArgs

  startMainWindow (headDef "/" args)

  mainGUI
