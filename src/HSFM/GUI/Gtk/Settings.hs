{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2016 Julian Ospald

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

{-# LANGUAGE PatternSynonyms #-}


module HSFM.GUI.Gtk.Settings where


import Graphics.UI.Gtk




    -----------------------
    --[ Common Settings ]--
    -----------------------



---- Hotkey settings ----


pattern QuitModifier :: [Modifier]
pattern QuitModifier <- [Control]

pattern QuitKey :: String
pattern QuitKey <- "q"


pattern ShowHiddenModifier :: [Modifier]
pattern ShowHiddenModifier <- [Control]

pattern ShowHiddenKey :: String
pattern ShowHiddenKey <- "h"


pattern UpDirModifier :: [Modifier]
pattern UpDirModifier <- [Alt]

pattern UpDirKey :: String
pattern UpDirKey <- "Up"


pattern HistoryBackModifier :: [Modifier]
pattern HistoryBackModifier <- [Alt]

pattern HistoryBackKey :: String
pattern HistoryBackKey <- "Left"


pattern HistoryForwardModifier :: [Modifier]
pattern HistoryForwardModifier <- [Alt]

pattern HistoryForwardKey :: String
pattern HistoryForwardKey <- "Right"


pattern DeleteModifier :: [Modifier]
pattern DeleteModifier <- []

pattern DeleteKey :: String
pattern DeleteKey <- "Delete"


pattern OpenModifier :: [Modifier]
pattern OpenModifier <- []

pattern OpenKey :: String
pattern OpenKey <- "Return"


pattern CopyModifier :: [Modifier]
pattern CopyModifier <- [Control]

pattern CopyKey :: String
pattern CopyKey <- "c"


pattern MoveModifier :: [Modifier]
pattern MoveModifier <- [Control]

pattern MoveKey :: String
pattern MoveKey <- "x"


pattern PasteModifier :: [Modifier]
pattern PasteModifier <- [Control]

pattern PasteKey :: String
pattern PasteKey <- "v"


pattern NewTabModifier :: [Modifier]
pattern NewTabModifier <- [Control]

pattern NewTabKey :: String
pattern NewTabKey <- "t"


pattern CloseTabModifier :: [Modifier]
pattern CloseTabModifier <- [Control]

pattern CloseTabKey :: String
pattern CloseTabKey <- "w"


pattern OpenTerminalModifier :: [Modifier]
pattern OpenTerminalModifier <- []

pattern OpenTerminalKey :: String
pattern OpenTerminalKey <- "F4"

