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

import Control.Applicative
  (
    (<$>)
  , (<*>)
  )
import Control.Concurrent
  (
    forkIO
  )
import Control.Concurrent.MVar
  (
    newEmptyMVar
  )
import Control.Concurrent.STM
  (
    TVar
  , newTVarIO
  , readTVarIO
  )
import Control.Exception
  (
    try
  , Exception
  , SomeException
  )
import Control.Monad
  (
    when
  , void
  )
import Control.Monad.IO.Class
  (
    liftIO
  )
import Data.DirTree
import Data.Foldable
  (
    for_
  )
import Data.List
  (
    sort
  , isPrefixOf
  )
import Data.Maybe
  (
    fromJust
  , catMaybes
  , fromMaybe
  )
import Data.Traversable
  (
    forM
  )
import Graphics.UI.Gtk
import GUI.Gtk.Callbacks
import GUI.Gtk.Data
import GUI.Gtk.Dialogs
import GUI.Gtk.Icons
import GUI.Gtk.Utils
import IO.Error
import IO.File
import IO.Utils
import MyPrelude
import Safe
  (
    headDef
  )
import System.Environment
  (
    getArgs
  )
import System.FilePath
  (
    isAbsolute
  , (</>)
  )
import System.Glib.UTFString
  (
    glibToString
  )
import System.IO.Unsafe
  (
    unsafePerformIO
  )
import System.Process
  (
    spawnProcess
  )


-- TODO: simplify where we modify the TVars
-- TODO: double check garbage collection/gtk ref counting
-- TODO: file watching, when and what to reread


main :: IO ()
main = do
  _ <- initGUI

  args <- getArgs

  startMainWindow (headDef "/" args)

  _ <- mainGUI
  return ()


    -------------------------
    --[ Main Window Setup ]--
    -------------------------


-- |Set up the GUI.
--
-- Interaction with mutable references:
--
-- * 'settings' creates
-- * 'operationBuffer' creates
-- * 'rawModel' creates
-- * 'filteredModel' creates
-- * 'sortedModel' creates
startMainWindow :: FilePath -> IO ()
startMainWindow startdir = do

  settings <- newTVarIO (MkFMSettings False True)

  inotify  <- newEmptyMVar

  -- get the icons
  iT           <- iconThemeGetDefault
  folderPix    <- getIcon IFolder iT 24
  folderSymPix <- getSymlinkIcon IFolder iT 24
  filePix      <- getIcon IFile iT 24
  fileSymPix   <- getSymlinkIcon IFile iT 24
  errorPix     <- getIcon IError iT 24

  operationBuffer <- newTVarIO None

  builder <- builderNew
  builderAddFromFile builder "data/Gtk/builder.xml"

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
  rcMenu            <- builderGetObject builder castToMenu
                       "rcMenu"
  rcFileOpen        <- builderGetObject builder castToImageMenuItem
                       "rcFileOpen"
  rcFileExecute     <- builderGetObject builder castToImageMenuItem
                       "rcFileExecute"
  rcFileNew         <- builderGetObject builder castToImageMenuItem
                       "rcFileNew"
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
  refreshView       <- builderGetObject builder castToButton
                       "refreshView"

  -- create initial list store model with unsorted data
  -- we check that the startdir passed by the user is valid
  -- TODO: maybe move this to a separate function
  sd <- (\x -> if (failed . file $ x) || (not . isAbsolute . anchor $ x)
                 then Data.DirTree.readFile "/"
                 else return x) =<< Data.DirTree.readFile startdir
  rawModel <- newTVarIO =<< listStoreNew
                        =<< Data.DirTree.getContents sd

  filteredModel <- newTVarIO =<< (\x -> treeModelFilterNew x [])
                           =<< readTVarIO rawModel

  -- create an initial sorting proxy model
  sortedModel <- newTVarIO =<< treeModelSortNewWithModel
                           =<< readTVarIO filteredModel

  -- create the final view
  treeView <- treeViewNew
  -- set selection mode
  tvs <- treeViewGetSelection treeView
  treeSelectionSetMode tvs SelectionMultiple

  -- create final tree model columns
  renderTxt <- cellRendererTextNew
  renderPix <- cellRendererPixbufNew
  let ct = cellText   :: (CellRendererTextClass cr) => Attr cr String
      cp = cellPixbuf :: (CellRendererPixbufClass self) => Attr self Pixbuf

  -- filename column
  cF <- treeViewColumnNew
  treeViewColumnSetTitle        cF "Filename"
  treeViewColumnSetResizable    cF True
  treeViewColumnSetClickable    cF True
  treeViewColumnSetSortColumnId cF 1
  cellLayoutPackStart cF renderPix False
  cellLayoutPackStart cF renderTxt True
  _ <- treeViewAppendColumn treeView cF
  cellLayoutAddColumnAttribute cF renderPix cp $ makeColumnIdPixbuf 0
  cellLayoutAddColumnAttribute cF renderTxt ct $ makeColumnIdString 1

  -- date column
  cMD <- treeViewColumnNew
  treeViewColumnSetTitle        cMD "Date"
  treeViewColumnSetResizable    cMD True
  treeViewColumnSetClickable    cMD True
  treeViewColumnSetSortColumnId cMD 2
  cellLayoutPackStart cMD renderTxt True
  _ <- treeViewAppendColumn treeView cMD
  cellLayoutAddColumnAttribute cMD renderTxt ct $ makeColumnIdString 2

  -- permissions column
  cP <- treeViewColumnNew
  treeViewColumnSetTitle        cP "Permission"
  treeViewColumnSetResizable    cP True
  treeViewColumnSetClickable    cP True
  treeViewColumnSetSortColumnId cP 3
  cellLayoutPackStart cP renderTxt True
  _ <- treeViewAppendColumn treeView cP
  cellLayoutAddColumnAttribute cP renderTxt ct $ makeColumnIdString 3

  -- construct the gui object
  let mygui  = MkMyGUI {..}
  let myview = MkMyView {..}

  -- create the tree model with its contents
  constructTreeView mygui myview

  -- set the bindings
  setCallbacks mygui myview

  -- add the treeview to the scroll container
  containerAdd scroll treeView

  -- sets the default icon
  windowSetDefaultIconFromFile "data/Gtk/icons/hsfm.png"

  widgetShowAll rootWin
