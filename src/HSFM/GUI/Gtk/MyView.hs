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



module HSFM.GUI.Gtk.MyView where


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
    forM_
  )
import qualified Data.ByteString as BS
import Data.Foldable
  (
    for_
  )
import Data.Maybe
  (
    catMaybes
  , fromJust
  )
import HPath.IO.Errors
  (
    canOpenDirectory
  )
import Graphics.UI.Gtk
import {-# SOURCE #-} HSFM.GUI.Gtk.Callbacks (setViewCallbacks)
import HPath
  (
    Path
  , Abs
  )
import qualified HPath as P
import HSFM.FileSystem.FileType
import HSFM.GUI.Glib.GlibString()
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Icons
import HSFM.GUI.Gtk.Utils
import HSFM.Utils.IO
import Paths_hsfm
  (
    getDataFileName
  )
import Prelude hiding(readFile)
import System.INotify
  (
    addWatch
  , initINotify
  , killINotify
  , EventVariety(..)
  )
import System.Posix.FilePath
  (
    pathSeparator
  , hiddenFile
  )



-- |Creates a new tab with its own view and refreshes the view.
newTab :: MyGUI -> IO FMView -> Path Abs -> IO MyView
newTab mygui iofmv path = do
  myview <- createMyView mygui iofmv
  i <- notebookAppendPage (notebook mygui) (viewBox myview)
       (maybe (P.fromAbs path) P.fromRel $ P.basename path)
  mpage <- notebookGetNthPage (notebook mygui) i
  forM_ mpage $ \page -> notebookSetTabReorderable (notebook mygui)
                                                   page
                                                   True
  refreshView mygui myview (Just path)
  return myview


-- |Constructs the initial MyView object with a few dummy models.
-- It also initializes the callbacks.
createMyView :: MyGUI
             -> IO FMView
             -> IO MyView
createMyView mygui iofmv = do
  inotify <- newEmptyMVar
  history <- newTVarIO ([],[])

  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName "data/Gtk/builder.xml"

  -- create dummy models, so we don't have to use MVar
  rawModel <- newTVarIO =<< listStoreNew []
  filteredModel <- newTVarIO =<< (\x -> treeModelFilterNew x [])
                             =<< readTVarIO rawModel
  sortedModel <- newTVarIO =<< treeModelSortNewWithModel
                           =<< readTVarIO filteredModel
  cwd <- newEmptyMVar
  view' <- iofmv
  view  <- newTVarIO view'

  urlBar            <- builderGetObject builder castToEntry
                       "urlBar"
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
  rcFileIconView    <- builderGetObject builder castToImageMenuItem
                       "rcFileIconView"
  rcFileTreeView    <- builderGetObject builder castToImageMenuItem
                       "rcFileTreeView"
  upViewB           <- builderGetObject builder castToButton
                       "upViewB"
  homeViewB         <- builderGetObject builder castToButton
                       "homeViewB"
  refreshViewB      <- builderGetObject builder castToButton
                       "refreshViewB"
  scroll            <- builderGetObject builder castToScrolledWindow
                       "mainScroll"
  viewBox           <- builderGetObject builder castToBox
                         "viewBox"

  let rcmenu = MkRightClickMenu {..}
  let myview = MkMyView {..}

  -- set the bindings
  setViewCallbacks mygui myview

  -- add the treeview to the scroll container
  let oview = fmViewToContainer view'
  containerAdd scroll oview

  widgetShowAll viewBox

  return myview


-- |Switch the existing view in `MyView` with the one that the
-- io action returns.
switchView :: MyGUI -> MyView -> IO FMView -> IO ()
switchView mygui myview iofmv = do
  cwd <- getCurrentDir myview

  oldpage <- destroyView mygui myview

  -- create new view and tab page where the previous one was
  nview <- createMyView mygui iofmv
  newpage <- notebookInsertPage (notebook mygui) (viewBox nview)
             (maybe (P.fromAbs $ path cwd) P.fromRel
                 $ P.basename . path $ cwd) oldpage
  notebookSetCurrentPage (notebook mygui) newpage

  refreshView' mygui nview cwd


-- |Destroys the current view by disconnecting the watcher
-- and destroying the active FMView container.
--
-- Everything that needs to be done in order to forget about a
-- view needs to be done here.
--
-- Returns the page in the tab list this view corresponds to.
destroyView :: MyGUI -> MyView -> IO Int
destroyView mygui myview = do
  -- disconnect watcher
  mi <- tryTakeMVar (inotify myview)
  for_ mi $ \i -> killINotify i

  page <- notebookGetCurrentPage (notebook mygui)

  -- destroy old view and tab page
  view' <- readTVarIO $ view myview
  widgetDestroy (fmViewToContainer view')
  notebookRemovePage (notebook mygui) page

  return page


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

  -- set drag and drop
  tl <- targetListNew
  atom <- atomNew ("HSFM" :: String)
  targetListAdd tl atom [TargetSameApp] 0
  treeViewEnableModelDragDest treeView tl [ActionCopy]
  treeViewEnableModelDragSource treeView [Button1] tl [ActionCopy]

  -- create final tree model columns
  renderTxt <- cellRendererTextNew
  renderPix <- cellRendererPixbufNew
  let ct = cellText   :: (CellRendererTextClass cr) => Attr cr String
      cp = cellPixbuf :: (CellRendererPixbufClass self) => Attr self Pixbuf

  -- filename column
  cF <- treeViewColumnNew
  treeViewColumnSetTitle        cF ("Filename" :: String)
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
  treeViewColumnSetTitle        cMD ("Date" :: String)
  treeViewColumnSetResizable    cMD True
  treeViewColumnSetClickable    cMD True
  treeViewColumnSetSortColumnId cMD 2
  cellLayoutPackStart cMD renderTxt True
  _ <- treeViewAppendColumn treeView cMD
  cellLayoutAddColumnAttribute cMD renderTxt ct $ makeColumnIdString 2

  -- permissions column
  cP <- treeViewColumnNew
  treeViewColumnSetTitle        cP ("Permission" :: String)
  treeViewColumnSetResizable    cP True
  treeViewColumnSetClickable    cP True
  treeViewColumnSetSortColumnId cP 3
  cellLayoutPackStart cP renderTxt True
  _ <- treeViewAppendColumn treeView cP
  cellLayoutAddColumnAttribute cP renderTxt ct $ makeColumnIdString 3

  return $ FMTreeView treeView


-- |Re-reads the current directory or the given one and updates the View.
-- This is more or less a wrapper around `refreshView'`
--
-- If the third argument is Nothing, it tries to re-read the current directory.
-- If that fails, it reads "/" instead.
--
-- If the third argument is (Just path) it tries to read "path". If that
-- fails, it reads "/" instead.
refreshView :: MyGUI
            -> MyView
            -> Maybe (Path Abs)
            -> IO ()
refreshView mygui myview mfp =
  case mfp of
    Just fp  -> do
      canopen <- canOpenDirectory fp
      if canopen
         then refreshView' mygui myview =<< readFile getFileInfo fp
         else refreshView mygui myview =<< getAlternativeDir
    Nothing  -> refreshView mygui myview =<< getAlternativeDir
    where
      getAlternativeDir = do
        ecd <- try (getCurrentDir myview) :: IO (Either SomeException
                                                     Item)
        case ecd of
          Right dir -> return (Just $ path dir)
          Left _    -> return (P.parseAbs $ BS.singleton pathSeparator)


-- |Refreshes the View based on the given directory.
--
-- If the directory is not a Dir or a Symlink pointing to a Dir, then
-- calls `refreshView` with the 3rd argument being Nothing.
refreshView' :: MyGUI
             -> MyView
             -> Item
             -> IO ()
refreshView' mygui myview SymLink { sdest = d@Dir{} } =
  refreshView' mygui myview d
refreshView' mygui myview item@Dir{} = do
  newRawModel  <- fileListStore item myview
  writeTVarIO (rawModel myview) newRawModel

  view' <- readTVarIO $ view myview

  _ <- tryTakeMVar (cwd myview)
  putMVar (cwd myview) item

  -- get selected items
  tps <- getSelectedTreePaths mygui myview
  trs <- catMaybes <$> mapM (treeRowReferenceNew newRawModel) tps

  constructView mygui myview

  -- set notebook tab label
  page <- notebookGetCurrentPage (notebook mygui)
  child <- fromJust <$> notebookGetNthPage (notebook mygui) page
  notebookSetTabLabelText (notebook mygui) child
       (maybe (P.fromAbs $ path item) P.fromRel $ P.basename . path $ item)

  -- reselect selected items
  -- TODO: not implemented for icon view yet
  case view' of
    FMTreeView treeView -> do
      tvs <- treeViewGetSelection treeView
      ntps <- mapM treeRowReferenceGetPath trs
      mapM_ (treeSelectionSelectPath tvs) ntps
    _ -> return ()
refreshView' mygui myview Failed{} = refreshView mygui myview Nothing
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
  let dirtreePix Dir{}           = folderPix
      dirtreePix FileLike{}      = filePix
      dirtreePix DirSym{}        = folderSymPix
      dirtreePix FileLikeSym{}   = fileSymPix
      dirtreePix Failed{}        = errorPix
      dirtreePix BrokenSymlink{} = errorPix
      dirtreePix _               = errorPix


  view' <- readTVarIO $ view myview

  cdirp <- path <$> getCurrentDir myview

  -- update urlBar
  entrySetText (urlBar myview) (P.fromAbs cdirp)

  rawModel' <- readTVarIO $ rawModel myview

  -- filtering
  filteredModel' <- treeModelFilterNew rawModel' []
  writeTVarIO (filteredModel myview) filteredModel'
  treeModelFilterSetVisibleFunc filteredModel' $ \iter -> do
     hidden <- showHidden <$> readTVarIO (settings mygui)
     item   <- treeModelGetRow rawModel' iter >>= (P.basename . path)
     if hidden
       then return True
       else return . not . hiddenFile . P.fromRel $ item

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
                     dirtreePix
  treeModelSetColumn rawModel' (makeColumnIdString 1)
                     (P.toFilePath . fromJust . P.basename . path)
  treeModelSetColumn rawModel' (makeColumnIdString 2)
                     packModTime
  treeModelSetColumn rawModel' (makeColumnIdString 3)
                     packPermissions

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
  _ <- addWatch
         newi
         [Move, MoveIn, MoveOut, MoveSelf, Create, Delete, DeleteSelf]
         (P.fromAbs cdirp)
         (\_ -> postGUIAsync $ refreshView mygui myview (Just $ cdirp))
  putMVar (inotify myview) newi

  return ()
