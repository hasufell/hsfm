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

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module HSFM.GUI.Gtk.MyGUI where


import Control.Concurrent.STM
  (
    newTVarIO
  )
import Graphics.UI.Gtk
import HSFM.GUI.Gtk.Data
import Paths_hsfm
  (
    getDataFileName
  )




    -------------------------
    --[ Main Window Setup ]--
    -------------------------


-- |Set up the GUI. This only creates the permanent widgets.
createMyGUI :: IO MyGUI
createMyGUI = do

  let settings' = MkFMSettings False True 24
  settings <- newTVarIO settings'

  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName "data/Gtk/builder.xml"

  -- get the pre-defined gui widgets
  rootWin           <- builderGetObject builder castToWindow
                       "rootWin"
  scroll            <- builderGetObject builder castToScrolledWindow
                       "mainScroll"
  menubarFileQuit   <- builderGetObject builder castToImageMenuItem
                       "menubarFileQuit"
  menubarFileOpen   <- builderGetObject builder castToImageMenuItem
                       "menubarFileOpen"
  menubarFileExecute <- builderGetObject builder castToImageMenuItem
                        "menubarFileExecute"
  menubarFileNew    <- builderGetObject builder castToImageMenuItem
                       "menubarFileNew"
  menubarEditCut    <- builderGetObject builder castToImageMenuItem
                       "menubarEditCut"
  menubarEditCopy   <- builderGetObject builder castToImageMenuItem
                       "menubarEditCopy"
  menubarEditRename <- builderGetObject builder castToImageMenuItem
                       "menubarEditRename"
  menubarEditPaste  <- builderGetObject builder castToImageMenuItem
                       "menubarEditPaste"
  menubarEditDelete <- builderGetObject builder castToImageMenuItem
                      "menubarEditDelete"
  menubarHelpAbout  <- builderGetObject builder castToImageMenuItem
                      "menubarHelpAbout"
  urlBar            <- builderGetObject builder castToEntry
                       "urlBar"
  statusBar         <- builderGetObject builder castToStatusbar
                       "statusBar"
  clearStatusBar    <- builderGetObject builder castToButton
                       "clearStatusBar"
  rcMenu            <- builderGetObject builder castToMenu
                       "rcMenu"
  rcFileOpen        <- builderGetObject builder castToImageMenuItem
                       "rcFileOpen"
  rcFileExecute     <- builderGetObject builder castToImageMenuItem
                       "rcFileExecute"
  rcFileNewRegFile  <- builderGetObject builder castToImageMenuItem
                       "rcFileNewRegFile"
  rcFileNewDir      <- builderGetObject builder castToImageMenuItem
                       "rcFileNewDir"
  rcFileCut         <- builderGetObject builder castToImageMenuItem
                       "rcFileCut"
  rcFileCopy        <- builderGetObject builder castToImageMenuItem
                       "rcFileCopy"
  rcFileRename      <- builderGetObject builder castToImageMenuItem
                       "rcFileRename"
  rcFilePaste       <- builderGetObject builder castToImageMenuItem
                       "rcFilePaste"
  rcFileDelete      <- builderGetObject builder castToImageMenuItem
                       "rcFileDelete"
  rcFileProperty    <- builderGetObject builder castToImageMenuItem
                       "rcFileProperty"
  upViewB           <- builderGetObject builder castToButton
                       "upViewB"
  homeViewB         <- builderGetObject builder castToButton
                       "homeViewB"
  refreshViewB      <- builderGetObject builder castToButton
                       "refreshViewB"
  menubarViewTree   <- builderGetObject builder castToImageMenuItem
                       "menubarViewTree"
  menubarViewIcon   <- builderGetObject builder castToImageMenuItem
                       "menubarViewIcon"
  fpropGrid         <- builderGetObject builder castToGrid
                       "fpropGrid"
  fpropFnEntry      <- builderGetObject builder castToEntry
                       "fpropFnEntry"
  fpropLocEntry     <- builderGetObject builder castToEntry
                       "fpropLocEntry"
  fpropTsEntry      <- builderGetObject builder castToEntry
                       "fpropTsEntry"
  fpropModEntry     <- builderGetObject builder castToEntry
                       "fpropModEntry"
  fpropAcEntry      <- builderGetObject builder castToEntry
                       "fpropAcEntry"
  fpropFTEntry      <- builderGetObject builder castToEntry
                       "fpropFTEntry"
  fpropPermEntry    <- builderGetObject builder castToEntry
                       "fpropPermEntry"
  fpropLDEntry      <- builderGetObject builder castToEntry
                       "fpropLDEntry"

  -- construct the gui object
  let menubar = MkMenuBar {..}
  let rcmenu = MkRightClickMenu {..}
  let fprop = MkFilePropertyGrid {..}
  let mygui  = MkMyGUI {..}

  -- sets the default icon
  _ <- windowSetDefaultIconFromFile
         =<< getDataFileName "data/Gtk/icons/hsfm.png"

  return mygui
