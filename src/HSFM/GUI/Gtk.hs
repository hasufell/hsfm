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

{-# OPTIONS_HADDOCK ignore-exports #-}

module Main where


import qualified Data.ByteString as BS
import Data.Maybe
  (
    fromJust
  , fromMaybe
  )
import Data.Word8
import Graphics.UI.Gtk
import qualified HPath as P
import HSFM.FileSystem.FileType
import HSFM.GUI.Gtk.Callbacks
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.MyGUI
import HSFM.GUI.Gtk.MyView
import Prelude hiding(readFile)
import Safe
  (
    headDef
  )
import System.IO.Error
  (
    catchIOError
  )
import qualified System.Posix.Env.ByteString as SPE

slash :: BS.ByteString
slash = BS.singleton _slash

main :: IO ()
main = do
  args <- SPE.getArgs
  let mdir = fromMaybe (fromJust $ P.parseAbs slash)
                       (P.parseAbs . headDef slash $ args)

  file <- catchIOError (pathToFile getFileInfo mdir) $
    \_ -> pathToFile getFileInfo  . fromJust $ P.parseAbs slash

  _ <- initGUI
  mygui <- createMyGUI
  _ <- newTab mygui (notebook1 mygui) createTreeView file (-1)
  _ <- newTab mygui (notebook2 mygui) createTreeView file (-1)

  setGUICallbacks mygui

  widgetShowAll (rootWin mygui)

  _ <- mainGUI
  return ()

