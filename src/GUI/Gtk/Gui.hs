{-# OPTIONS_HADDOCK ignore-exports #-}

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
import Data.DirTree.Zipper
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
import GUI.Gtk.Icons
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
  -- |tree view
  , treeView :: TreeView
  -- |first column
  , cF :: TreeViewColumn
  -- |second column
  , cMD :: TreeViewColumn
  -- |renderer used for the treeView
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


-- |This describes the contents of the treeView and is separated from MyGUI,
-- because we might want to have multiple views.
data MyView = MkMyView {
  -- |raw model with unsorted data
    rawModel :: TVar (ListStore DTInfoZipper)
  -- |sorted proxy model
  , sortedModel :: TVar (TypedTreeModelSort DTInfoZipper)
  -- |filtered proxy model
  , filteredModel :: TVar (TypedTreeModelFilter DTInfoZipper)
  , fsState :: TVar DTInfoZipper
  , operationBuffer :: TVar (Either
                         (DTInfoZipper -> FileOperation)
                         FileOperation)
}


-- |Set callbacks, on hotkeys, events and stuff.
setCallbacks :: MyGUI -> MyView -> IO ()
setCallbacks mygui myview = do
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- fmap glibToString eventKeyName
    liftIO mainQuit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "h"       <- fmap glibToString eventKeyName
    liftIO $ modifyTVarIO (settings mygui)
                          (\x -> x { showHidden = not . showHidden $ x})
             >> (refreshTreeView' mygui myview =<< readTVarIO (fsState myview))
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Alt] <- eventModifier
    "Up"  <- fmap glibToString eventKeyName
    liftIO $ upDir mygui myview
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    "Delete"  <- fmap glibToString eventKeyName
    liftIO $ withRow mygui myview del
  _ <- treeView mygui `on` rowActivated $ (\_ _ -> withRow mygui myview open)
  _ <- menubarFileQuit mygui `on` menuItemActivated $ mainQuit
  _ <- urlBar mygui `on` entryActivated $ urlGoTo mygui myview
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "c"       <- fmap glibToString eventKeyName
    liftIO $ withRow mygui myview copyInit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "v"       <- fmap glibToString eventKeyName
    liftIO $ copyFinal mygui myview
  return ()


-- |Go to the url given at the `urlBar` and visualize it in the given
-- treeView.
--
-- This might update the TVar `rawModel`.
urlGoTo :: MyGUI -> MyView -> IO ()
urlGoTo mygui myview = do
  fp <- entryGetText (urlBar mygui)
  let abs = isAbsolute fp
  exists <- (||) <$> doesDirectoryExist fp <*> doesFileExist fp
  -- TODO: more explicit error handling?
  refreshTreeView mygui myview (Just fp)


-- |Gets the currently selected row of the treeView, if any.
getSelectedRow :: MyGUI
               -> MyView
               -> IO (Maybe DTInfoZipper)
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
        -> (   DTInfoZipper
            -> MyGUI
            -> MyView
            -> IO ()) -- ^ action to carry out
        -> IO ()
withRow mygui myview io = do
  mrow <- getSelectedRow mygui myview
  for_ mrow $ \row -> io row mygui myview


-- |Supposed to be used with `withRow`. Opens a file or directory.
open :: DTInfoZipper -> MyGUI -> MyView -> IO ()
open row mygui myview = case row of
  (Dir {}, _) ->
    refreshTreeView' mygui myview row
  dz@(File {}, _) ->
    withErrorDialog $ openFile dz
  _ -> return ()


-- |Supposed to be used with `withRow`. Deletes a file or directory.
del :: DTInfoZipper -> MyGUI -> MyView -> IO ()
del row mygui myview = case row of
  dz@(Dir {}, _)   -> do
    let fp   = getFullPath dz
        cmsg = "Really delete directory \"" ++ fp ++ "\"?"
    withConfirmationDialog cmsg
      $ withErrorDialog (deleteDir dz
                          >> refreshTreeView mygui myview Nothing)
  dz@(File {}, _) -> do
    let fp   = getFullPath dz
        cmsg = "Really delete file \"" ++ fp ++ "\"?"
    withConfirmationDialog cmsg
      $ withErrorDialog (deleteFile dz
                          >> refreshTreeView mygui myview Nothing)


-- |Supposed to be used with `withRow`. Initializes a file copy operation.
copyInit :: DTInfoZipper -> MyGUI -> MyView -> IO ()
copyInit row mygui myview = case row of
  dz -> writeTVarIO (operationBuffer myview) (Left $ FCopy dz)


-- |Finalizes a file copy operation.
copyFinal :: MyGUI -> MyView -> IO ()
copyFinal mygui myview = do
  mOp <- readTVarIO (operationBuffer myview)
  op <- case mOp of
    Left pOp -> do
      curDir <- readTVarIO (fsState myview)
      case pOp curDir of
        op@(FCopy _ _) -> return op
        _              -> return None
    Right op@(FCopy _ _) -> return op
    _ -> return None
  doCopy op
  where
    doCopy op@(FCopy from to) = do
      let cmsg = "Really copy file \"" ++ getFullPath from
                 ++ "\"" ++ " to \"" ++ getFullPath to ++ "\"?"
      withConfirmationDialog cmsg
        $ withErrorDialog
          (runFileOp op >> refreshTreeView mygui myview Nothing)
    doCopy _ = return ()


-- |Go up one directory and visualize it in the treeView.
upDir :: MyGUI -> MyView -> IO ()
upDir mygui myview = do
  rawModel'    <- readTVarIO $ rawModel myview
  sortedModel' <- readTVarIO $ sortedModel myview
  fS           <- readTVarIO $ fsState myview
  refreshTreeView' mygui myview (goUp fS)


-- |Create the `ListStore` of files/directories from the current directory.
-- This is the function which maps the Data.DirTree data structures
-- into the GTK+ data structures.
--
-- This also updates the TVar `fsState` inside the given view.
fileListStore :: DTInfoZipper  -- ^ current dir
              -> MyView
              -> IO (ListStore DTInfoZipper)
fileListStore dtz myview = do
  writeTVarIO (fsState myview) dtz
  listStoreNew (goAllDown dtz)


-- |Re-reads the current directory or the given one and updates the TreeView.
-- This means that the DTZipper is re-initialized.
-- If you can operate on the raw DTZipper directly, use `refreshTreeView'`
-- instead.
--
-- This also updates the TVar `rawModel`.
--
-- This throws exceptions via `dirSanityThrow` if the given/current
-- directory path does not exist.
refreshTreeView :: MyGUI
                -> MyView
                -> Maybe FilePath
                -> IO ()
refreshTreeView mygui myview mfp = do
  fsState <- readTVarIO $ fsState myview
  let cfp = getFullPath fsState
      fp  = fromMaybe cfp mfp

  -- TODO catch exceptions
  dirSanityThrow fp

  newFsState  <- readPath' fp
  newRawModel <- fileListStore newFsState myview
  writeTVarIO (rawModel myview) newRawModel
  constructTreeView mygui myview


-- |Refreshes the TreeView based on the given Zipper.
--
-- This also updates the TVar `rawModel`.
refreshTreeView' :: MyGUI
                 -> MyView
                 -> DTInfoZipper
                 -> IO ()
refreshTreeView' mygui myview dtz = do
  newRawModel  <- fileListStore dtz myview
  writeTVarIO (rawModel myview) newRawModel
  constructTreeView mygui myview


-- TODO: make this function more slim so only the most necessary parts are
-- called
-- |Constructs the visible TreeView with the current underlying mutable models,
-- which are retrieved from `MyGUI`.
--
-- This also updates the TVars `filteredModel` and `sortedModel` in the process.
constructTreeView :: MyGUI
                  -> MyView
                  -> IO ()
constructTreeView mygui myview = do
  let treeView' = treeView mygui
      cF' = cF mygui
      cMD' = cMD mygui
      render' = renderTxt mygui

  -- update urlBar, this will break laziness slightly, probably
  fsState <- readTVarIO $ fsState myview
  let urlpath = getFullPath fsState
  entrySetText (urlBar mygui) urlpath

  rawModel' <- readTVarIO $ rawModel myview

  -- filtering
  filteredModel' <- treeModelFilterNew rawModel' []
  writeTVarIO (filteredModel myview) filteredModel'
  treeModelFilterSetVisibleFunc filteredModel' $ \iter -> do
     hidden <- showHidden <$> readTVarIO (settings mygui)
     row    <- treeModelGetRow rawModel' iter
     if hidden
       then return True
       else return $ not ("." `isPrefixOf` (name . unZip $ row))

  -- sorting
  sortedModel' <- treeModelSortNewWithModel filteredModel'
  writeTVarIO (sortedModel myview) sortedModel'
  treeSortableSetSortFunc sortedModel' 1 $ \iter1 iter2 -> do
      cIter1 <- treeModelFilterConvertIterToChildIter filteredModel' iter1
      cIter2 <- treeModelFilterConvertIterToChildIter filteredModel' iter2
      row1   <- treeModelGetRow rawModel' cIter1
      row2   <- treeModelGetRow rawModel' cIter2
      return $ compare (unZip row1) (unZip row2)
  treeSortableSetSortColumnId sortedModel' 1 SortAscending

  -- set values
  treeModelSetColumn rawModel' (makeColumnIdPixbuf 0)
                               (dirtreePix . unZip)
  treeModelSetColumn rawModel' (makeColumnIdString 1)
                               (name . unZip)
  treeModelSetColumn rawModel' (makeColumnIdString 2)
                               (packModTime . unZip)
  treeModelSetColumn rawModel' (makeColumnIdString 3)
                               (packPermissions . unZip)

  -- update treeview model
  treeViewSetModel treeView' sortedModel'

  return ()
  where
    dirtreePix (Dir {})    = folderPix mygui
    dirtreePix (File {})   = filePix mygui
    dirtreePix (Failed {}) = errorPix mygui


pushStatusBar :: MyGUI -> String -> IO (ContextId, MessageId)
pushStatusBar mygui str = do
  let sb = statusBar mygui
  cid <- statusbarGetContextId sb "FM Status"
  mid <- statusbarPush sb cid str
  return (cid, mid)


-- |Pops up an error Dialog with the given String.
showErrorDialog :: String -> IO ()
showErrorDialog str = do
  errorDialog <- messageDialogNew Nothing
                                  [DialogDestroyWithParent]
                                  MessageError
                                  ButtonsClose
                                  str
  _ <- dialogRun errorDialog
  widgetDestroy errorDialog


showConfirmationDialog :: String -> IO Bool
showConfirmationDialog str = do
  errorDialog <- messageDialogNew Nothing
                                  [DialogDestroyWithParent]
                                  MessageQuestion
                                  ButtonsYesNo
                                  str
  rID <- dialogRun errorDialog
  widgetDestroy errorDialog
  case rID of
    ResponseYes -> return True
    ResponseNo  -> return False
    _           -> return False


withConfirmationDialog :: String -> IO () -> IO ()
withConfirmationDialog str io = do
  run <- showConfirmationDialog str
  when run io


-- |Execute the given IO action. If the action throws exceptions,
-- visualize them via `showErrorDialog`.
withErrorDialog :: IO a -> IO ()
withErrorDialog io = do
  r <- try io
  either (\e -> showErrorDialog $ show (e :: SomeException))
         (\_ -> return ())
         r


-- |Set up the GUI.
startMainWindow :: FilePath -> IO ()
startMainWindow startdir = do

  settings <- newTVarIO (MkFMSettings False True)

  -- get the icons
  iT        <- iconThemeGetDefault
  folderPix <- getIcon IFolder 24
  filePix   <- getIcon IFile 24
  errorPix  <- getIcon IError 24

  fsState <- readPath' startdir >>= newTVarIO

  operationBuffer <- newTVarIO (Right None)

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
  rawModel <- newTVarIO =<< listStoreNew . goAllDown =<< readTVarIO fsState

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
