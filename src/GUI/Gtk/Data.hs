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


import Control.Concurrent.MVar
  (
    MVar
  )
import Control.Concurrent.STM
  (
    TVar
  )
import Data.DirTree
import Graphics.UI.Gtk
import IO.File
import System.INotify
  (
    INotify
  )



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
  , menubarViewTree :: ImageMenuItem
  , menubarViewIcon :: ImageMenuItem
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
  , refreshViewB :: Button
  , urlBar :: Entry
  , statusBar :: Statusbar
  , clearStatusBar :: Button
  , settings :: TVar FMSettings
  , folderPix :: Pixbuf
  , folderSymPix :: Pixbuf
  , filePix :: Pixbuf
  , fileSymPix :: Pixbuf
  , errorPix :: Pixbuf
  , scroll :: ScrolledWindow
}


-- |FM-wide settings.
data FMSettings = MkFMSettings {
    showHidden :: Bool
  , isLazy :: Bool
}

data FMView = FMTreeView TreeView
            | FMIconView IconView

type Item = AnchoredFile FileInfo


-- |This describes the contents of the current vie and is separated from MyGUI,
-- because we might want to have multiple views.
data MyView = MkMyView {
    view            :: TVar FMView
  , rawModel        :: TVar (ListStore Item)
  , sortedModel     :: TVar (TypedTreeModelSort Item)
  , filteredModel   :: TVar (TypedTreeModelFilter Item)
  , operationBuffer :: TVar FileOperation
  , inotify         :: MVar INotify
}


fmViewToContainer :: FMView -> Container
fmViewToContainer (FMTreeView x) =  castToContainer . toGObject $ x
fmViewToContainer (FMIconView x) =  castToContainer . toGObject $ x
