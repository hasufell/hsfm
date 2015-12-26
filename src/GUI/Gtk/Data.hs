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

module GUI.Gtk.Data where


import Control.Concurrent.STM
  (
    TVar
  )
import Data.DirTree
import Graphics.UI.Gtk
import IO.File




    ------------------
    --[ Base Types ]--
    ------------------


-- |Monolithic object passed to various GUI functions in order
-- to keep the API stable and not alter the parameters too much.
-- This only holds GUI widgets that are needed to be read during
-- runtime.
data MyGUI = MkMyGUI {
  -- |main Window
    rootWin  :: Window
  , menubarFileQuit :: ImageMenuItem
  , menubarFileOpen :: ImageMenuItem
  , menubarFileExecute :: ImageMenuItem
  , menubarFileNew :: ImageMenuItem
  , menubarEditCut :: ImageMenuItem
  , menubarEditCopy :: ImageMenuItem
  , menubarEditRename :: ImageMenuItem
  , menubarEditPaste :: ImageMenuItem
  , menubarEditDelete :: ImageMenuItem
  , menubarHelpAbout :: ImageMenuItem
  , rcMenu :: Menu
  , rcFileOpen :: ImageMenuItem
  , rcFileExecute :: ImageMenuItem
  , rcFileNew :: ImageMenuItem
  , rcFileCut :: ImageMenuItem
  , rcFileCopy :: ImageMenuItem
  , rcFileRename :: ImageMenuItem
  , rcFilePaste :: ImageMenuItem
  , rcFileDelete :: ImageMenuItem
  , refreshView :: Button
  , urlBar :: Entry
  , statusBar :: Statusbar
  , treeView :: TreeView
  -- |first column
  , cF :: TreeViewColumn
  -- |second column
  , cMD :: TreeViewColumn
  , renderTxt :: CellRendererText
  , renderPix :: CellRendererPixbuf
  , settings :: TVar FMSettings
  , folderPix :: Pixbuf
  , filePix :: Pixbuf
  , errorPix :: Pixbuf
}


-- |FM-wide settings.
data FMSettings = MkFMSettings {
    showHidden :: Bool
  , isLazy :: Bool
}


type Row = AnchoredFile FileInfo


-- |This describes the contents of the treeView and is separated from MyGUI,
-- because we might want to have multiple views.
data MyView = MkMyView {
    rawModel :: TVar (ListStore Row)
  , sortedModel :: TVar (TypedTreeModelSort Row)
  , filteredModel :: TVar (TypedTreeModelFilter Row)
  , operationBuffer :: TVar FileOperation
}

