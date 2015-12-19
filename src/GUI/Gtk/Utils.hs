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
  )
import Data.Traversable
  (
    forM
  )
import Graphics.UI.Gtk
import GUI.Gtk.Data
import IO.Error
import IO.Utils


import qualified Data.IntMap.Lazy as IM



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
--
-- Interaction with mutable references:
--
-- * 'fsState' writes
fileListStore :: AnchoredDirFile FileInfo FileInfo  -- ^ current dir
              -> MyView
              -> IO (ListStore Row)
fileListStore dt myview = do
  writeTVarIO (fsState myview) dt
  listStoreNew (IM.keys . dirTree $ dt)


-- |Re-reads the current directory or the given one and updates the TreeView.
--
-- The operation may fail with:
--
-- * 'DirDoesNotExist' if the target directory does not exist
-- * 'PathNotAbsolute' if the target directory is not absolute
--
-- Interaction with mutable references:
--
-- * 'fsState' reads
-- * 'rawModel' writes
refreshTreeView :: MyGUI
                -> MyView
                -> Maybe FilePath
                -> IO ()
refreshTreeView mygui myview mfp = do
  fsState <- readTVarIO $ fsState myview
  let cfp = anchor fsState
      fp  = fromMaybe cfp mfp

  -- TODO catch exceptions
  dirSanityThrow fp

  newFsState  <- readPath fp
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
                 -> AnchoredDirFile FileInfo FileInfo
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
-- * 'fsState' reads
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

  fsState <- readTVarIO $ fsState myview
  let dirL = dirLookup fsState

  -- update urlBar, this will break laziness slightly, probably
  let urlpath = anchor fsState
  entrySetText (urlBar mygui) urlpath

  rawModel' <- readTVarIO $ rawModel myview

  -- filtering
  filteredModel' <- treeModelFilterNew rawModel' []
  writeTVarIO (filteredModel myview) filteredModel'
  treeModelFilterSetVisibleFunc filteredModel' $ \iter -> do
     hidden <- showHidden <$> readTVarIO (settings mygui)
     row    <- (name . dirL) <$> treeModelGetRow rawModel' iter
     if hidden
       then return True
       else return $ not ("." `isPrefixOf` row)

  -- sorting
  sortedModel' <- treeModelSortNewWithModel filteredModel'
  writeTVarIO (sortedModel myview) sortedModel'
  treeSortableSetSortFunc sortedModel' 1 $ \iter1 iter2 -> do
      cIter1 <- treeModelFilterConvertIterToChildIter filteredModel' iter1
      cIter2 <- treeModelFilterConvertIterToChildIter filteredModel' iter2
      row1   <- dirL <$> treeModelGetRow rawModel' cIter1
      row2   <- dirL <$> treeModelGetRow rawModel' cIter2
      return $ compare row1 row2
  treeSortableSetSortColumnId sortedModel' 1 SortAscending

  -- set values
  treeModelSetColumn rawModel' (makeColumnIdPixbuf 0)
                     (dirtreePix . dirL)
  treeModelSetColumn rawModel' (makeColumnIdString 1)
                     (name . dirL)
  treeModelSetColumn rawModel' (makeColumnIdString 2)
                     (packModTime . dirL)
  treeModelSetColumn rawModel' (makeColumnIdString 3)
                     (packPermissions . dirL)

  -- update treeview model
  treeViewSetModel treeView' sortedModel'

  return ()
  where
    dirtreePix (Dir {})    = folderPix mygui
    dirtreePix (File {})   = filePix mygui
    dirtreePix (Failed {}) = errorPix mygui


-- |Push a message to the status bar.
pushStatusBar :: MyGUI -> String -> IO (ContextId, MessageId)
pushStatusBar mygui str = do
  let sb = statusBar mygui
  cid <- statusbarGetContextId sb "FM Status"
  mid <- statusbarPush sb cid str
  return (cid, mid)
