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

{-# OPTIONS_HADDOCK ignore-exports #-}

module HSFM.GUI.Gtk.Utils where


import Control.Concurrent.MVar
  (
    readMVar
  )
import Control.Concurrent.STM
  (
    readTVarIO
  )
import Data.Maybe
  (
    catMaybes
  , fromJust
  )
import Data.Traversable
  (
    forM
  )
import Graphics.UI.Gtk
import HSFM.FileSystem.FileType
import HSFM.GUI.Gtk.Data
import Prelude hiding(getContents)



    -----------------
    --[ Utilities ]--
    -----------------


getSelectedTreePaths :: MyGUI -> MyView -> IO [TreePath]
getSelectedTreePaths _ myview = do
  view' <- readTVarIO $ view myview
  case view' of
    FMTreeView treeView -> do
      tvs   <- treeViewGetSelection treeView
      treeSelectionGetSelectedRows tvs
    FMIconView iconView ->
      iconViewGetSelectedItems iconView


-- |Gets the currently selected item of the treeView, if any.
getSelectedItems :: MyGUI
                 -> MyView
                 -> IO [Item]
getSelectedItems mygui myview = do
  tps   <- getSelectedTreePaths mygui myview
  getSelectedItems' mygui myview tps


getSelectedItems' :: MyGUI
                  -> MyView
                  -> [TreePath]
                  -> IO [Item]
getSelectedItems' _ myview tps = do
  rawModel'      <- readTVarIO $ rawModel myview
  sortedModel'   <- readTVarIO $ sortedModel myview
  filteredModel' <- readTVarIO $ filteredModel myview
  iters <- catMaybes <$> mapM (treeModelGetIter sortedModel') tps
  forM iters $ \iter -> do
    cIter' <- treeModelSortConvertIterToChildIter sortedModel' iter
    cIter  <- treeModelFilterConvertIterToChildIter filteredModel' cIter'
    treeModelGetRow rawModel' cIter




-- |Carry out an action on the currently selected item.
--
-- If there is no item selected, does nothing.
withItems :: MyGUI
          -> MyView
          -> (   [Item]
              -> MyGUI
              -> MyView
              -> IO ()) -- ^ action to carry out
          -> IO ()
withItems mygui myview io = do
  items <- getSelectedItems mygui myview
  io items mygui myview


-- |Create the 'ListStore' of files/directories from the current directory.
-- This is the function which maps the Data.DirTree data structures
-- into the GTK+ data structures.
fileListStore :: Item  -- ^ current dir
              -> MyView
              -> IO (ListStore Item)
fileListStore dt _ = do
  cs <- getContents getFileInfo dt
  listStoreNew cs


-- |Currently unsafe. This is used to obtain any item, which will
-- fail if there is none.
getFirstItem :: MyView
             -> IO Item
getFirstItem myview = do
  rawModel' <- readTVarIO $ rawModel myview
  iter      <- fromJust <$> treeModelGetIterFirst rawModel'
  treeModelGetRow rawModel' iter


-- |Reads the current directory from MyView.
getCurrentDir :: MyView
              -> IO Item
getCurrentDir myview = readMVar (cwd myview)




-- |Push a message to the status bar.
pushStatusBar :: MyGUI -> String -> IO (ContextId, MessageId)
pushStatusBar mygui str = do
  let sb = statusBar mygui
  cid <- statusbarGetContextId sb "FM Status"
  mid <- statusbarPush sb cid str
  return (cid, mid)


-- |Pop a message from the status bar.
popStatusbar :: MyGUI -> IO ()
popStatusbar mygui = do
  let sb = statusBar mygui
  cid <- statusbarGetContextId sb "FM Status"
  statusbarPop sb cid
