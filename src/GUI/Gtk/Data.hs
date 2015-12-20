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
  , menubarFileCut :: ImageMenuItem
  , menubarFileCopy :: ImageMenuItem
  , menubarFilePaste :: ImageMenuItem
  , menubarFileDelete :: ImageMenuItem
  , menubarHelpAbout :: ImageMenuItem
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


type Row = AnchoredFile FileInfo FileInfo


-- |This describes the contents of the treeView and is separated from MyGUI,
-- because we might want to have multiple views.
data MyView = MkMyView {
    rawModel :: TVar (ListStore Row)
  , sortedModel :: TVar (TypedTreeModelSort Row)
  , filteredModel :: TVar (TypedTreeModelFilter Row)
  , fsState :: TVar (AnchoredFile FileInfo FileInfo)
  , operationBuffer :: TVar FileOperation
}

