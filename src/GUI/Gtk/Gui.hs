{-# OPTIONS_HADDOCK ignore-exports #-}

-- |The main Gtk Gui module for creating the main window.
module GUI.Gtk.Gui (startMainWindow) where


import Control.Applicative
  (
    (<$>)
  , (<*>)
  )
import Control.Concurrent
  (
    forkIO
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
import System.Directory
  (
    executable
  , doesFileExist
  , doesDirectoryExist
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


import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM


-- TODO: simplify where we modify the TVars
-- TODO: double check garbage collection/gtk ref counting
-- TODO: file watching, when and what to reread





    -------------------------
    --[ Main Window Setup ]--
    -------------------------


-- |Set up the GUI.
--
-- Interaction with mutable references:
--
-- * 'settings' creates
-- * 'fsState' creates
-- * 'operationBuffer' creates
-- * 'rawModel' creates
-- * 'filteredModel' creates
-- * 'sortedModel' creates
startMainWindow :: FilePath -> IO ()
startMainWindow startdir = do

  settings <- newTVarIO (MkFMSettings False True)

  -- get the icons
  iT        <- iconThemeGetDefault
  folderPix <- getIcon IFolder 24
  filePix   <- getIcon IFile 24
  errorPix  <- getIcon IError 24

  fsState <- readPath startdir >>= newTVarIO

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
  menubarFileCut    <- builderGetObject builder castToImageMenuItem
                       "menubarFileCut"
  menubarFileCopy   <- builderGetObject builder castToImageMenuItem
                       "menubarFileCopy"
  menubarFilePaste  <- builderGetObject builder castToImageMenuItem
                       "menubarFilePaste"
  menubarFileDelete <- builderGetObject builder castToImageMenuItem
                      "menubarFileDelete"
  menubarHelpAbout  <- builderGetObject builder castToImageMenuItem
                      "menubarHelpAbout"
  urlBar            <- builderGetObject builder castToEntry
                       "urlBar"
  statusBar         <- builderGetObject builder castToStatusbar
                       "statusBar"

  -- create initial list store model with unsorted data
  rawModel <- newTVarIO =<< listStoreNew . IM.keys . dirTree
                        =<< readTVarIO fsState

  filteredModel <- newTVarIO =<< (\x -> treeModelFilterNew x [])
                           =<< readTVarIO rawModel

  -- create an initial sorting proxy model
  sortedModel <- newTVarIO =<< treeModelSortNewWithModel
                           =<< readTVarIO filteredModel

  -- create the final view
  treeView <- treeViewNew

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

  widgetShowAll rootWin
