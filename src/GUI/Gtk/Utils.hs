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

module GUI.Gtk.Utils where


import Control.Applicative
  (
    (<$>)
  )
import Control.Concurrent.STM
  (
    TVar
  , newTVarIO
  , readTVarIO
  )
import Data.DirTree
import Data.Foldable
  (
    for_
  )
import Data.List
  (
    isPrefixOf
  )
import Data.Maybe
  (
    fromMaybe
  , fromJust
  )
import Data.Traversable
  (
    forM
  )
import Graphics.UI.Gtk
import GUI.Gtk.Data
import IO.Error
import IO.Utils
import MyPrelude




    -----------------
    --[ Utilities ]--
    -----------------


-- |Gets the currently selected row of the treeView, if any.
--
-- Interaction with mutable references:
--
-- * 'rawModel' reads
-- * 'sortedModel' reads
-- * 'filteredModel' reads
getSelectedRow :: MyGUI
               -> MyView
               -> IO (Maybe Row)
getSelectedRow mygui myview = do
  (tp, _)        <- treeViewGetCursor $ treeView mygui
  rawModel'      <- readTVarIO $ rawModel myview
  sortedModel'   <- readTVarIO $ sortedModel myview
  filteredModel' <- readTVarIO $ filteredModel myview
  miter          <- treeModelGetIter sortedModel' tp
  forM miter $ \iter -> do
    cIter' <- treeModelSortConvertIterToChildIter sortedModel' iter
    cIter  <- treeModelFilterConvertIterToChildIter filteredModel' cIter'
    treeModelGetRow rawModel' cIter


-- |Carry out an action on the currently selected row.
--
-- If there is no row selected, does nothing.
withRow :: MyGUI
        -> MyView
        -> (   Row
            -> MyGUI
            -> MyView
            -> IO ()) -- ^ action to carry out
        -> IO ()
withRow mygui myview io = do
  mrow <- getSelectedRow mygui myview
  for_ mrow $ \row -> io row mygui myview


-- |Create the 'ListStore' of files/directories from the current directory.
-- This is the function which maps the Data.DirTree data structures
-- into the GTK+ data structures.
fileListStore :: AnchoredFile FileInfo  -- ^ current dir
              -> MyView
              -> IO (ListStore Row)
fileListStore dt myview = do
  cs <- Data.DirTree.getContents dt
  listStoreNew cs


-- |Currently unsafe. This is used to obtain any row (possibly the '.' row)
-- and extract the "current working directory" from it.
--
-- Interaction with mutable references:
--
-- * 'rawModel' reads
getFirstRow :: MyView
            -> IO (AnchoredFile FileInfo)
getFirstRow myview = do
  rawModel' <- readTVarIO $ rawModel myview
  iter      <- fromJust <$> treeModelGetIterFirst rawModel'
  treeModelGetRow rawModel' iter


-- |Currently unsafe. Gets the current directory via `getFirstRow` and `goUp`.
getCurrentDir :: MyView
              -> IO (AnchoredFile FileInfo)
getCurrentDir myview = getFirstRow myview >>= goUp


-- |Re-reads the current directory or the given one and updates the TreeView.
--
-- The operation may fail with:
--
-- * 'DirDoesNotExist' if the target directory does not exist
-- * 'PathNotAbsolute' if the target directory is not absolute
--
-- Interaction with mutable references:
--
-- * 'rawModel' writes
refreshTreeView :: MyGUI
                -> MyView
                -> Maybe FilePath
                -> IO ()
refreshTreeView mygui myview mfp = do
  mcdir <- getFirstRow myview
  let fp  = fromMaybe (anchor mcdir) mfp

  -- TODO catch exceptions
  dirSanityThrow fp

  newFsState  <- Data.DirTree.readFile fp
  newRawModel <- fileListStore newFsState myview
  writeTVarIO (rawModel myview) newRawModel
  constructTreeView mygui myview


-- |Refreshes the TreeView based on the given directory.
--
-- Interaction with mutable references:
--
-- * 'rawModel' writes
refreshTreeView' :: MyGUI
                 -> MyView
                 -> AnchoredFile FileInfo
                 -> IO ()
refreshTreeView' mygui myview dt = do
  newRawModel  <- fileListStore dt myview
  writeTVarIO (rawModel myview) newRawModel
  constructTreeView mygui myview


-- TODO: make this function more slim so only the most necessary parts are
-- called
-- |Constructs the visible TreeView with the current underlying mutable models,
-- which are retrieved from 'MyGUI'.
--
-- Interaction with mutable references:
--
-- * 'rawModel' reads
-- * 'filteredModel' writes
-- * 'sortedModel' writes
-- * 'settings' reads
constructTreeView :: MyGUI
                  -> MyView
                  -> IO ()
constructTreeView mygui myview = do
  let treeView' = treeView mygui
      cF' = cF mygui
      cMD' = cMD mygui
      render' = renderTxt mygui

  mcdir <- getFirstRow myview

  -- update urlBar
  entrySetText (urlBar mygui) (anchor mcdir)

  rawModel' <- readTVarIO $ rawModel myview

  -- filtering
  filteredModel' <- treeModelFilterNew rawModel' []
  writeTVarIO (filteredModel myview) filteredModel'
  treeModelFilterSetVisibleFunc filteredModel' $ \iter -> do
     hidden <- showHidden <$> readTVarIO (settings mygui)
     row    <- (name . file) <$> treeModelGetRow rawModel' iter
     if hidden
       then return True
       else return $ not . hiddenFile $ row

  -- sorting
  sortedModel' <- treeModelSortNewWithModel filteredModel'
  writeTVarIO (sortedModel myview) sortedModel'
  treeSortableSetSortFunc sortedModel' 1 $ \iter1 iter2 -> do
      cIter1 <- treeModelFilterConvertIterToChildIter filteredModel' iter1
      cIter2 <- treeModelFilterConvertIterToChildIter filteredModel' iter2
      row1   <- treeModelGetRow rawModel' cIter1
      row2   <- treeModelGetRow rawModel' cIter2
      return $ compare row1 row2
  treeSortableSetSortColumnId sortedModel' 1 SortAscending

  -- set values
  treeModelSetColumn rawModel' (makeColumnIdPixbuf 0)
                     (dirtreePix . file)
  treeModelSetColumn rawModel' (makeColumnIdString 1)
                     (name . file)
  treeModelSetColumn rawModel' (makeColumnIdString 2)
                     (packModTime . file)
  treeModelSetColumn rawModel' (makeColumnIdString 3)
                     (packPermissions . file)

  -- update treeview model
  treeViewSetModel treeView' sortedModel'

  return ()
  where
    dirtreePix (Dir {})     = folderPix mygui
    dirtreePix (RegFile {}) = filePix mygui
    dirtreePix (Failed {})  = errorPix mygui
    dirtreePix _  = errorPix mygui


-- |Push a message to the status bar.
pushStatusBar :: MyGUI -> String -> IO (ContextId, MessageId)
pushStatusBar mygui str = do
  let sb = statusBar mygui
  cid <- statusbarGetContextId sb "FM Status"
  mid <- statusbarPush sb cid str
  return (cid, mid)
