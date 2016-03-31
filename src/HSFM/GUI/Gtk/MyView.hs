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

module HSFM.GUI.Gtk.MyView where


import Control.Applicative
  (
    (<$>)
  )
import Control.Concurrent.MVar
  (
    newEmptyMVar
  , putMVar
  , tryTakeMVar
  )
import Control.Concurrent.STM
  (
    newTVarIO
  , readTVarIO
  )
import Control.Exception
  (
    try
  , SomeException
  )
import Control.Monad
  (
    when
  )
import Data.Foldable
  (
    for_
  )
import Data.Maybe
  (
    catMaybes
  , fromJust
  , fromMaybe
  )
import Graphics.UI.Gtk
import {-# SOURCE #-} HSFM.GUI.Gtk.Callbacks (setCallbacks)
import qualified HPath as P
import HSFM.FileSystem.FileOperations
import HSFM.FileSystem.FileType
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Icons
import HSFM.GUI.Gtk.Utils
import HSFM.Utils.IO
import System.FilePath
  (
    isAbsolute
  )
import System.INotify
  (
    addWatch
  , initINotify
  , killINotify
  , EventVariety(..)
  , Event(..)
  )
import System.IO.Error
  (
    tryIOError
  )




-- |Constructs the initial MyView object with a few dummy models.
-- It also initializes the callbacks.
createMyView :: MyGUI -> IO FMView -> IO MyView
createMyView mygui iofmv = do
  operationBuffer <- newTVarIO None

  inotify <- newEmptyMVar

  -- create dummy models, so we don't have to use MVar
  rawModel <- newTVarIO =<< listStoreNew []
  filteredModel <- newTVarIO =<< (\x -> treeModelFilterNew x [])
                             =<< readTVarIO rawModel
  sortedModel <- newTVarIO =<< treeModelSortNewWithModel
                           =<< readTVarIO filteredModel

  view' <- iofmv
  view  <- newTVarIO view'

  let myview = MkMyView {..}

  -- set the bindings
  setCallbacks mygui myview

  -- add the treeview to the scroll container
  let oview = fmViewToContainer view'
  containerAdd (scroll mygui) oview

  return myview


-- |Switch the existing view in `MyView` with the one that the
-- io action returns.
switchView :: MyGUI -> MyView -> IO FMView -> IO ()
switchView mygui myview iofmv = do
  view' <- readTVarIO $ view myview
  let oview = fmViewToContainer view'

  widgetDestroy oview

  nview' <- iofmv
  let nview = fmViewToContainer nview'

  writeTVarIO (view myview) nview'

  setCallbacks mygui myview

  containerAdd (scroll mygui) nview
  widgetShow nview

  refreshView mygui myview Nothing


-- |Createss an IconView.
createIconView :: IO FMView
createIconView = do
  iconv <- iconViewNew
  iconViewSetSelectionMode iconv SelectionMultiple
  iconViewSetColumns iconv (-1)
  iconViewSetSpacing iconv 2
  iconViewSetMargin iconv 0
  {- set iconv [ iconViewItemOrientation := OrientationHorizontal ] -}
  {- set iconv [ iconViewOrientation := OrientationHorizontal ] -}

  return $ FMIconView iconv


-- |Creates a TreeView.
createTreeView :: IO FMView
createTreeView = do
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

  return $ FMTreeView treeView


-- |Re-reads the current directory or the given one and updates the View.
--
-- If the third argument is Nothing, it tries to re-read the current directory.
-- If that fails, it reads "/" instead.
--
-- If the third argument is (Just path) it tries to read "path". If that
-- fails, it reads "/" instead.
refreshView :: MyGUI
            -> MyView
            -> Maybe FilePath
            -> IO ()
refreshView mygui myview mfp =
  case mfp of
    Just fp  -> do
      let mdir = fromMaybe (fromJust $ P.parseAbs "/") (P.parseAbs fp)
      -- readFileWithFileInfo can just outright fail...
      ecdir <- tryIOError (HSFM.FileSystem.FileType.readFileWithFileInfo mdir)
      case ecdir of
        Right cdir -> do
          -- ...or return an `AnchordFile` with a Failed constructor,
          -- both of which need to be handled here
          if (failed . file $ cdir)
            then refreshView mygui myview =<< getAlternativeDir
            else refreshView' mygui myview cdir
        Left e -> refreshView mygui myview =<< getAlternativeDir
    Nothing  -> refreshView mygui myview =<< getAlternativeDir
    where
      getAlternativeDir = do
        ecd <- try (getCurrentDir myview) :: IO (Either SomeException
                                                        (AnchoredFile FileInfo))
        case (ecd) of
          Right dir -> return (Just $ fullPathS dir)
          Left _    -> return (Just "/")


-- |Refreshes the View based on the given directory.
--
-- If the directory is not a Dir or a Symlink pointing to a Dir, then
-- calls `refreshView` with the 3rd argument being Nothing.
refreshView' :: MyGUI
             -> MyView
             -> AnchoredFile FileInfo
             -> IO ()
refreshView' mygui myview dt@(ADirOrSym _) = do
  newRawModel  <- fileListStore dt myview
  writeTVarIO (rawModel myview) newRawModel

  view' <- readTVarIO $ view myview

  -- get selected items
  tps <- getSelectedTreePaths mygui myview
  trs <- catMaybes <$> mapM (treeRowReferenceNew newRawModel) tps

  constructView mygui myview

  -- reselect selected items
  -- TODO: not implemented for icon view yet
  case view' of
    FMTreeView treeView -> do
      tvs <- treeViewGetSelection treeView
      ntps <- mapM treeRowReferenceGetPath trs
      mapM_ (treeSelectionSelectPath tvs) ntps
    _ -> return ()
refreshView' mygui myview (_ :/ Failed{}) = refreshView mygui myview Nothing
refreshView' _ _ _ = return ()


-- |Constructs the visible View with the current underlying mutable models,
-- which are retrieved from 'MyGUI'.
--
-- This sort of merges the components mygui and myview and fires up
-- the actual models.
constructView :: MyGUI
              -> MyView
              -> IO ()
constructView mygui myview = do
  settings' <- readTVarIO $ settings mygui

  -- pix stuff
  iT           <- iconThemeGetDefault
  folderPix    <- getIcon IFolder iT (iconSize settings')
  folderSymPix <- getSymlinkIcon IFolder iT (iconSize settings')
  filePix      <- getIcon IFile iT (iconSize settings')
  fileSymPix   <- getSymlinkIcon IFile iT (iconSize settings')
  errorPix     <- getIcon IError iT (iconSize settings')
  let dirtreePix (Dir {})          = folderPix
      dirtreePix (FileLike {})     = filePix
      dirtreePix (DirSym _)        = folderSymPix
      dirtreePix (FileLikeSym {})  = fileSymPix
      dirtreePix (Failed {})       = errorPix
      dirtreePix (BrokenSymlink _) = errorPix
      dirtreePix _                 = errorPix


  view' <- readTVarIO $ view myview

  cdirp <- anchor <$> getFirstItem myview

  -- update urlBar
  entrySetText (urlBar mygui) (P.fromAbs cdirp)

  rawModel' <- readTVarIO $ rawModel myview

  -- filtering
  filteredModel' <- treeModelFilterNew rawModel' []
  writeTVarIO (filteredModel myview) filteredModel'
  treeModelFilterSetVisibleFunc filteredModel' $ \iter -> do
     hidden <- showHidden <$> readTVarIO (settings mygui)
     item    <- (name . file) <$> treeModelGetRow rawModel' iter
     if hidden
       then return True
       else return $ not . hiddenFile $ item

  -- sorting
  sortedModel' <- treeModelSortNewWithModel filteredModel'
  writeTVarIO (sortedModel myview) sortedModel'
  treeSortableSetSortFunc sortedModel' 1 $ \iter1 iter2 -> do
      cIter1 <- treeModelFilterConvertIterToChildIter filteredModel' iter1
      cIter2 <- treeModelFilterConvertIterToChildIter filteredModel' iter2
      item1  <- treeModelGetRow rawModel' cIter1
      item2  <- treeModelGetRow rawModel' cIter2
      return $ compare item1 item2
  treeSortableSetSortColumnId sortedModel' 1 SortAscending

  -- set values
  treeModelSetColumn rawModel' (makeColumnIdPixbuf 0)
                     (dirtreePix . file)
  treeModelSetColumn rawModel' (makeColumnIdString 1)
                     (P.fromRel . name . file)
  treeModelSetColumn rawModel' (makeColumnIdString 2)
                     (packModTime . file)
  treeModelSetColumn rawModel' (makeColumnIdString 3)
                     (packPermissions . file)

  -- update model of view
  case view' of
    FMTreeView treeView -> do
      treeViewSetModel treeView sortedModel'
      treeViewSetRubberBanding treeView True
    FMIconView iconView -> do
      iconViewSetModel iconView (Just sortedModel')
      iconViewSetPixbufColumn iconView
                              (makeColumnIdPixbuf 0 :: ColumnId item Pixbuf)
      iconViewSetTextColumn iconView
                            (makeColumnIdString 1 :: ColumnId item String)

  -- add watcher
  mi <- tryTakeMVar (inotify myview)
  for_ mi $ \i -> killINotify i
  newi <- initINotify
  w <- addWatch
         newi
         [Move, MoveIn, MoveOut, MoveSelf, Create, Delete, DeleteSelf]
         (P.fromAbs cdirp)
         (\_ -> postGUIAsync $ refreshView mygui myview (Just $ P.fromAbs cdirp))
  putMVar (inotify myview) newi

  return ()