{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2015 Julian Ospald

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Main where


import Data.Maybe
  (
    fromJust
  , fromMaybe
  )
import Graphics.UI.Gtk
import qualified HPath as P
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.MyGUI
import HSFM.GUI.Gtk.MyView
import Safe
  (
    headDef
  )
import qualified System.Posix.Env.ByteString as SPE


main :: IO ()
main = do
  _ <- initGUI

  args <- SPE.getArgs

  mygui <- createMyGUI

  myview <- createMyView mygui createTreeView

  let mdir = fromMaybe (fromJust $ P.parseAbs "/")
                       (P.parseAbs . headDef "/" $ args)
  refreshView mygui myview (Just $ mdir)

  widgetShowAll (rootWin mygui)

  _ <- mainGUI
  return ()

